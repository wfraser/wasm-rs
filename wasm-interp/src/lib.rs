#[macro_use] extern crate log;

extern crate wasm_binary;
use wasm_binary::Module;
use wasm_binary::module::{self, ValueType};
use wasm_binary::instructions::{self, Instruction};

use std::collections::HashMap;
use std::io;

const PAGE_SIZE: usize = 64 * 1024;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    BinaryFormat(&'static str, u64),
    Instantiation(&'static str),
    MissingImport { module: String, field: String },
    InvalidOperation(&'static str),
    Runtime(&'static str),
}

impl From<wasm_binary::Error> for Error {
    fn from(other: wasm_binary::Error) -> Error {
        match other {
            wasm_binary::Error::IO(e) => Error::IO(e),
            wasm_binary::Error::Invalid(e) => Error::BinaryFormat(e, 0),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    pub fn valuetype(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
        }
    }
}

fn boolean(b: bool) -> Value {
    if b {
        Value::I32(1)
    } else {
        Value::I32(0)
    }
}

/// Helper function to aid in making the right boxed function type, because rustc's type inference
/// makes it awkward otherwise.
pub fn make_import_function<F>(f: F) -> ImportFunction
    where F: Fn(Vec<Value>) -> Option<Value> + 'static,
{
    Box::new(f)
}

pub type ImportFunction = Box<Fn(Vec<Value>) -> Option<Value>>;

#[derive(Debug)]
pub struct Function {
    signature: module::FuncType,
    definition: FunctionDefinition,
}

enum FunctionDefinition {
    Internal {
        locals: Vec<module::LocalEntry>,
        instructions: Vec<Instruction>,
    },
    Import(ImportFunction),
}

impl ::std::fmt::Debug for FunctionDefinition {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            FunctionDefinition::Internal { locals, instructions } => {
                f.debug_struct("FunctionDefinition::Internal")
                    .field("locals", locals)
                    .field("instructions", instructions)
                    .finish()
            }
            FunctionDefinition::Import(_) => {
                f.write_str("FunctionDefinition::Import(_)\n")
            }
        }
    }
}

pub struct HostEnvironment {
    pub functions: HashMap<String, ImportFunction>,
}

pub struct MutableState {
    pub memory: Vec<u8>,
    pub globals: Vec<GlobalEntry>,
}

pub struct GlobalEntry {
    value: Value,
    mutable: bool,
}

#[derive(Debug)]
struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            values: vec![],
        }
    }

    pub fn push(&mut self, v: Value) {
        self.values.push(v);
    }

    pub fn pop(&mut self) -> Result<Value, Error> {
        match self.values.pop() {
            Some(value) => Ok(value),
            None => Err(Error::Runtime("stack underflow")),
        }
    }

    pub fn last(&self) -> Result<Value, Error> {
        match self.values.last() {
            Some(value) => Ok(value.clone()),
            None => Err(Error::Runtime("stack underflow")),
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

/// Pop from the stack and expect a certain value type.
macro_rules! popt {
    ($stack:expr, $t:path) => {
        match $stack.pop()? {
            $t(v) => Ok(v),
            _ => Err(Error::Runtime("type mismatch")),
        }
    }
}

macro_rules! binop_bool {
    ($stack:expr, $valty:path, $cast:ty, $op:tt) => {
        {
            let b = popt!($stack, $valty)? as $cast;
            let a = popt!($stack, $valty)? as $cast;
            let c = a $op b;
            debug!(concat!("{} ", stringify!($op), " {} = {}"), a, b, c);
            $stack.push(boolean(c));
        }
    }
}

macro_rules! binop {
    ($stack:expr, $valty_in:path, $cast_in:tt, $op:tt, $valty_out:path, $cast_out:tt) => {
        {
            let b = popt!($stack, $valty_in)? as $cast_in;
            let a = popt!($stack, $valty_in)? as $cast_in;
            let c = a $op b;
            debug!(concat!("{} ", stringify!($op), " {} = {}"), a, b, c);
            $stack.push($valty_out(c as $cast_out));
        }
    }
}

macro_rules! binop_m {
    ($stack:expr, $valty_in:path, $cast_in:tt, $method:tt, $valty_out:path, $cast_out:tt) => {
        {
            let b = popt!($stack, $valty_in)? as $cast_in;
            let a = popt!($stack, $valty_in)? as $cast_in;
            let c = a.$method(b);
            debug!(concat!("{} ", stringify!($method), " {} = {}"), a, b, c);
            $stack.push($valty_out(c as $cast_out));
        }
    }
}

#[derive(Debug)]
struct ControlEntry {
    label: Label,
    stack_limit: usize,
    signature: module::BlockType,
}

#[derive(Debug)]
enum Label {
    Unbound,
    Bound(usize),
}

pub struct ModuleEnvironment {
    types: Vec<module::FuncType>,
    functions: Vec<Function>,
    start: Option<usize>,
    exports: HashMap<String, (module::ExternalKind, usize)>,
    symtab: Option<wasm_binary::name_section::SymbolTable>,
}

impl ModuleEnvironment {
    fn function_name(&self, index: usize) -> Option<&str> {
        let name = self.symtab.as_ref()?
            .functions.as_ref()?
            .get(&index).as_ref()?
            .name.as_ref()?
            .as_str();
        Some(name)
    }

    fn local_name(&self, fn_index: usize, local_index: usize) -> Option<&str> {
        let name = self.symtab.as_ref()?
            .functions.as_ref()?
            .get(&fn_index).as_ref()?
            .locals.as_ref()?
            .get(&local_index).as_ref()?
            .as_str();
        Some(name)
    }

    fn zero_value(typ: ValueType) -> Value {
        match typ {
            ValueType::I32 => Value::I32(0),
            ValueType::I64 => Value::I64(0),
            ValueType::F32 => Value::F32(0.),
            ValueType::F64 => Value::F64(0.),
        }
    }

    pub fn call_function(&mut self, name: &str, args: &[Value], mutable_state: &mut MutableState)
        -> Result<Option<Value>, Error>
    {
        let (kind, idx) = self.exports
            .get(name)
            .cloned()
            .ok_or(Error::InvalidOperation("no such export"))?;
        if kind != module::ExternalKind::Function {
            return Err(Error::InvalidOperation("export is not a function"));
        }
        info!("public calling function {:?}, index {}", name, idx);
        let mut stack = Stack::new();

        let sig = self.functions[idx].signature.clone();
        debug!("signature: {:?}", sig);

        if sig.param_types.len() != args.len() {
            return Err(Error::InvalidOperation("wrong number of arguments"));
        }
        for (typ, arg) in sig.param_types.iter().zip(args) {
            debug!("arg: {:?} - {:?}", typ, arg);
            if arg.valuetype() != *typ {
                return Err(Error::InvalidOperation("wrong argument type(s)"));
            }
        }
        for arg in args.into_iter().rev() {
            stack.push(*arg);
        }

        self.call(idx, &mut stack, mutable_state)?;

        if let Some(return_type) = sig.return_type {
            if stack.len() != 1 {
                // TODO: should this be a hard error?
                error!("expected 1 value, but stack is: {:?}", stack);
                stack.pop().map(Some)
            } else {
                let val = stack.pop().unwrap();
                if val.valuetype() != return_type {
                    error!("returned type is wrong type. Expected {:?}, got {:?}",
                        return_type, val);
                }
                Ok(Some(val))
            }
        } else {
            if !stack.is_empty() {
                // TODO: should this be a hard error? or is it normal?
                error!("expected empty stack, but stack is: {:?}", stack);
            }
            Ok(None)
        }
    }

    fn call(&self, idx: usize, stack: &mut Stack, mutable_state: &mut MutableState)
        -> Result<(), Error>
    {
        let f = self.functions.get(idx).ok_or(Error::Runtime("function index out of range"))?;

        if let Some(name) = self.function_name(idx) {
            info!("running function {}", name);
        } else {
            info!("running unnamed function {}", idx);
        }

        match &f.definition {
            FunctionDefinition::Internal { locals: ref local_specs, ref instructions } => {
                info!("internal function, {:?}, locals: {:?}", f.signature, local_specs);

                // Set up the locals with zero values of the appropriate types and counts
                let mut locals = vec![];
                for param_type in &f.signature.param_types {
                    debug!("adding empty {:?} arg value as local {} {:?}",
                        param_type,
                        locals.len(),
                        self.local_name(idx, locals.len()).unwrap_or("<unnamed>"));
                    locals.push(Self::zero_value(*param_type));
                }
                for local in local_specs {
                    for _ in 0 .. local.count {
                        debug!("adding empty {:?} value as local {} {:?}",
                            local.typ,
                            locals.len(),
                            self.local_name(idx, locals.len()).unwrap_or("<unnamed>"));
                        locals.push(Self::zero_value(local.typ));
                    }
                }

                // Load parameters from the stack into locals
                let num_locals = f.signature.param_types.len();
                for i in 0 .. num_locals {
                    let val = stack.pop()?;
                    let slot = locals.get_mut(num_locals - i - 1).unwrap();
                    if val.valuetype() != slot.valuetype() {
                        return Err(Error::Runtime("parameter type mismatch"));
                    }
                    debug!("Setting local {} to parameter value {:?}", num_locals - i - 1, val);
                    *slot = val;
                }

                // Fresh control stack with entry for the function itself as a whole.
                let mut control = vec![ControlEntry {
                    label: Label::Bound(instructions.len()),
                    stack_limit: 0,
                    signature: module::BlockType::from(f.signature.return_type)
                }];

                // Callee gets its own fresh value stack.
                let mut callee_stack = Stack::new();

                self.call_internal(
                    instructions, &mut callee_stack, mutable_state, &mut control, &mut locals)?;

                if let Some(typ) = f.signature.return_type {
                    // Last value on the stack is the return value.
                    while callee_stack.len() > 1 {
                        callee_stack.pop().unwrap();
                    }
                    let value = callee_stack.pop()?;
                    info!("Returning {:?}", value);

                    if value.valuetype() != typ {
                        return Err(Error::Runtime("wrong value type returned from function"));
                    }

                    stack.push(value);
                } else {
                    info!("Returning void");
                }
            }
            FunctionDefinition::Import(lambda) => {
                info!("imported function, {:?}", f.signature);
                Self::call_import(&f.signature, lambda, stack, mutable_state)?;
            }
        }

        Ok(())
    }

    fn call_internal(
        &self,
        instructions: &[Instruction],
        stack: &mut Stack,
        state: &mut MutableState,
        control: &mut Vec<ControlEntry>,
        locals: &mut [Value],
        ) -> Result<(), Error>
    {
        // TODO: it would be nice to have the instruction offset available for debugging
        let mut ip = 0;
        loop {
            let inst = instructions.get(ip)
                .ok_or(Error::Runtime("instruction pointer out of bounds"))?;

            trace!("{}: {:?}", ip, inst);
            match inst {
                Instruction::Unreachable => {
                    error!("entered unreachable code!");
                    return Err(Error::Runtime("entered unreachable code"));
                }
                Instruction::Nop => (),
                Instruction::Block(return_type) => {
                    control.push(ControlEntry {
                        label: Label::Unbound,
                        stack_limit: stack.len(),
                        signature: *return_type,
                    });
                }
                Instruction::Loop(return_type) => {
                    control.push(ControlEntry {
                        label: Label::Bound(ip),
                        stack_limit: stack.len(),
                        signature: *return_type,
                    });
                }
                Instruction::If(_return_type) => {
                    //TODO
                    unimplemented!();
                }
                Instruction::Else => {
                    //TODO
                    unimplemented!();
                }
                Instruction::End => {
                    let entry = control.pop()
                        .ok_or(Error::Runtime("control stack underflow"))?;
                    debug!("popped control entry {:?}", entry);

                    // TODO: bind the label to here and store this in the original branch
                    // instruction somehow.

                    if control.is_empty() {
                        info!("End of function");
                        match entry.label {
                            Label::Bound(n) => {
                                if n != ip + 1 {
                                    error!("ip = {}; entry = {:?}", ip, entry);
                                    return Err(Error::Runtime("function control entry doesn't match the end point"));
                                }
                            }
                            Label::Unbound => {
                                return Err(Error::Runtime("topmost control entry has unbound label"));
                            }
                        }
                        return Ok(());
                    }
                }
                Instruction::Br(num_entries) => {
                    debug!("Branch {} stack entries", num_entries);
                    ip = Self::branch(stack, control, instructions, ip, *num_entries)?;
                }
                Instruction::BrIf(num_entries) => {
                    let cond = popt!(stack, Value::I32)? != 0;
                    debug!("conditional branch ({:?}) {} stack entries", cond, num_entries);
                    if cond {
                        ip = Self::branch(stack, control, instructions, ip, *num_entries)?;
                    }
                }
                Instruction::BrTable { target_table, default_target } => {
                    debug!("BranchTable: ({:?}) default = {}", target_table, default_target);
                    //TODO
                    unimplemented!();
                }
                Instruction::Call(idx) => {
                    debug!("Call {}", idx);
                    self.call(*idx as usize, stack, state)?;
                }
                Instruction::CallIndirect(type_idx) => {
                    // pop, interpret as fn index, compare signatures
                    let signature = self.types.get(*type_idx as usize)
                        .ok_or(Error::Runtime("type index out of bounds for indirect call"))?;
                    debug!("CallIndirect with type signature {:?}", signature);
                    //TODO
                    unimplemented!();
                }
                Instruction::Return => {
                    debug!("Returning from function");
                    return Ok(());
                }
                Instruction::Drop => {
                    let dropped = stack.pop()?;
                    debug!("Drop: {:?}", dropped);
                }
                Instruction::Select => {
                    let cond = stack.pop()?;
                    let b = stack.pop()?;
                    let a = stack.pop()?;
                    debug!("Select: {:?} ? {:?} : {:?}", cond, a, b);
                    if a.valuetype() != b.valuetype() {
                        return Err(Error::Runtime("mismatching operand types for select instruction"));
                    }
                    let result = match cond {
                        Value::I32(0) => b,
                        Value::I32(_) => a,
                        _ => {
                            return Err(Error::Runtime("non-I32 condition argument to select instruction"));
                        }
                    };
                    stack.push(result);
                }

                Instruction::GetLocal(idx) => {
                    let val = locals.get(*idx)
                        .ok_or(Error::Runtime("attempt to get a local out of bounds"))?
                        .clone();
                    debug!("get local {}: {:?}", idx, val);
                    stack.push(val);
                }
                Instruction::SetLocal(idx) => {
                    let val = stack.pop()?;
                    debug!("set local {} to {:?}", idx, val);
                    let local: &mut Value = locals.get_mut(*idx)
                        .ok_or(Error::Runtime("attempt to set a local out of bounds"))?;
                    *local = val;
                }
                Instruction::TeeLocal(idx) => {
                    let val = stack.last()?;
                    debug!("set local {} to {:?}", idx, val);
                    let local: &mut Value = locals.get_mut(*idx)
                        .ok_or(Error::Runtime("attempt to set a local out of bounds"))?;
                    *local = val;
                }
                Instruction::GetGlobal(idx) => {
                    let entry = state.globals.get(*idx as usize)
                        .ok_or(Error::Runtime("attempt to get a global out of bounds"))?;
                    debug!("got global {}: {:?}", idx, entry.value);
                    stack.push(entry.value);
                }
                Instruction::SetGlobal(idx) => {
                    let entry = state.globals.get_mut(*idx as usize)
                        .ok_or(Error::Runtime("attempt to set a global out of bounds"))?;
                    if !entry.mutable {
                        return Err(Error::Runtime("attempt to set an immutable global"));
                    }
                    let value = stack.pop()?;
                    debug!("setting global {} from {:?} to {:?}", idx, entry.value, value);
                    entry.value = value;
                }

                Instruction::I32Load(arg) => {
                    let mut value = 0u32;

                    // TODO: what about the alignment?
                    let offset = arg.offset as usize;
                    let base = popt!(stack, Value::I32)? as usize;
                    let addr = base + offset;

                    for i in 0 .. 4 {
                        value |= (state.memory[addr + i] as u32) << (8 * i);
                    }
                    debug!("loaded {}i32 from ({:#x}+{:#x}={:#x})", value as i32, base, offset, addr);
                    stack.push(Value::I32(value as i32));
                }

                // ...

                Instruction::I32Load8S(arg) => {
                    let offset = arg.offset as usize;
                    let base = popt!(stack, Value::I32)? as usize;
                    let addr = base + offset;
                    let value = state.memory[addr] as i8 as i32;
                    debug!("loaded {}u8 from ({:#x}+{:#x}={:#x})", value, base, offset, addr);
                    stack.push(Value::I32(value));
                }
                Instruction::I32Load8U(arg) => {
                    let offset = arg.offset as usize;
                    let base = popt!(stack, Value::I32)? as usize;
                    let addr = base + offset;
                    let value = state.memory[addr] as u32;
                    debug!("loaded {}u8 from ({:#x}+{:#x}={:#x})", value, base, offset, addr);
                    stack.push(Value::I32(value as i32));
                }

                // ...

                Instruction::I32Store(arg) => {
                    let value = popt!(stack, Value::I32)?;
                    let base = popt!(stack, Value::I32)? as usize;
                    let offset = arg.offset as usize;
                    let addr = base + offset;
                    debug!("storing {} to ({:#x}+{:#x}={:#x})", value, base, offset, addr);
                    for i in 0 .. 4 {
                        state.memory[addr + i] = ((value & (0xFF << (8 * i))) >> (8 * i)) as u8;
                    }
                }
                Instruction::I64Store(arg) => {
                    let value = popt!(stack, Value::I64)?;
                    let base = popt!(stack, Value::I32)? as usize;
                    let offset = arg.offset as usize;
                    let addr = base + offset;
                    debug!("storing {} to ({:#x}+{:#x}={:#x})", value, base, offset, addr);
                    for i in 0 .. 8 {
                        state.memory[addr + i] = ((value & (0xFF << (8 * i))) >> (8 * i)) as u8;
                    }
                }

                // ...

                Instruction::I32Store8(arg) => {
                    let value = popt!(stack, Value::I32)? as u8;
                    let base = popt!(stack, Value::I32)? as usize;
                    let offset = arg.offset as usize;
                    let addr = base + offset;
                    debug!("storing {} to ({:#x}+{:#x}={:#x})", value, base, offset, addr);
                    state.memory[addr] = value;
                }

                // ...

                Instruction::I32Const(value) => {
                    stack.push(Value::I32(*value));
                }
                Instruction::I64Const(value) => {
                    stack.push(Value::I64(*value));
                }

                // ...

                Instruction::I32Eqz => {
                    let x = popt!(stack, Value::I32)?;
                    debug!("{} == 0 = {:?}", x, x == 0);
                    stack.push(boolean(x == 0));
                }
                Instruction::I32Eq  => binop_bool!(stack, Value::I32, i32, ==),
                Instruction::I32Ne  => binop_bool!(stack, Value::I32, i32, !=),
                Instruction::I32LtS => binop_bool!(stack, Value::I32, i32, <),
                Instruction::I32LtU => binop_bool!(stack, Value::I32, u32, <),
                Instruction::I32GtS => binop_bool!(stack, Value::I32, i32, >),
                Instruction::I32GtU => binop_bool!(stack, Value::I32, u32, >),
                Instruction::I32LeS => binop_bool!(stack, Value::I32, i32, <=),
                Instruction::I32LeU => binop_bool!(stack, Value::I32, u32, <=),
                Instruction::I32GeS => binop_bool!(stack, Value::I32, i32, >=),
                Instruction::I32GeU => binop_bool!(stack, Value::I32, u32, >=),

                // ...

                Instruction::I32Add  => binop!(stack, Value::I32, i32, +, Value::I32, i32),
                Instruction::I32Sub  => binop!(stack, Value::I32, i32, -, Value::I32, i32),
                Instruction::I32Mul  => binop!(stack, Value::I32, i32, *, Value::I32, i32),
                Instruction::I32DivS => binop!(stack, Value::I32, i32, /, Value::I32, i32),
                Instruction::I32DivU => binop!(stack, Value::I32, u32, /, Value::I32, i32),

                // ...

                Instruction::I32Clz => {
                    let x = popt!(stack, Value::I32)?;
                    let n = x.leading_zeros();
                    debug!("{} leading zeroes = {}", x, n);
                    stack.push(Value::I32(n as i32));
                }
                Instruction::I32Ctz => {
                    let x = popt!(stack, Value::I32)?;
                    let n = x.trailing_zeros();
                    debug!("{} trailing zeroes = {}", x, n);
                    stack.push(Value::I32(n as i32));
                }
                Instruction::I32PopCnt => {
                    let x = popt!(stack, Value::I32)?;
                    let n = x.count_ones();
                    debug!("{} popcnt = {}", x, n);
                    stack.push(Value::I32(n as i32));
                }
                Instruction::I32And => binop!(stack, Value::I32, i32, &, Value::I32, i32),
                Instruction::I32Or  => binop!(stack, Value::I32, i32, |, Value::I32, i32),
                Instruction::I32Xor => binop!(stack, Value::I32, i32, ^, Value::I32, i32),
                Instruction::I32Shl => binop!(stack, Value::I32, i32, <<, Value::I32, i32),
                Instruction::I32ShrS => binop!(stack, Value::I32, i32, >>, Value::I32, i32),
                Instruction::I32ShrU => binop!(stack, Value::I32, u32, >>, Value::I32, i32),
                Instruction::I32Rotl => binop_m!(stack, Value::I32, u32, rotate_left, Value::I32, i32),
                Instruction::I32Rotr => binop_m!(stack, Value::I32, u32, rotate_right, Value::I32, i32),

                // ...

                _ => {
                    error!("unimplemented instruction {:?}", inst);
                    //TODO
                    unimplemented!();
                }
            }

            ip += 1;
        }
    }

    fn call_import(
        signature: &module::FuncType,
        lambda: &ImportFunction,
        stack: &mut Stack,
        _mutable_state: &mut MutableState,
        ) -> Result<(), Error>
    {
        let mut args = vec![];
        for typ in &signature.param_types {
            if let Ok(arg) = stack.pop() {
                if arg.valuetype() != *typ {
                    return Err(Error::Runtime(
                            "wrong argument type on the stack for imported function"));
                }
                args.push(arg);
            } else {
                return Err(Error::Runtime(
                        "not enough values on the stack for arguments to imported
                        function"));
            }
        }

        let result = lambda(args);

        match (result, signature.return_type) {
            (Some(val), Some(typ)) if val.valuetype() == typ => {
                stack.push(val);
            }
            (None, None) => (),
            (Some(_), Some(_)) => {
                return Err(Error::Runtime(
                        "imported function returned a value of the wrong type"));
            }
            (Some(_), None) => {
                return Err(Error::Runtime(
                        "imported function returned a value but was not supposed to"));
            }
            (None, Some(_)) => {
                return Err(Error::Runtime(
                        "imported function did not return a value but was supposed to"));
            }
        }

        Ok(())
    }

    fn branch(
        stack: &mut Stack,
        control: &mut Vec<ControlEntry>,
        instructions: &[Instruction],
        ip: usize,
        num_entries: u32,
        ) -> Result<usize, Error>
    {
        for _ in 0 .. num_entries {
            if control.pop().is_none() {
                return Err(Error::Runtime("control stack underflow"));
            }
        }

        let entry = control.last()
            .ok_or(Error::Runtime("control stack underflow"))?;
        debug!("control stack entry: {:?}", entry);

        while stack.len() > entry.stack_limit {
            let value = stack.pop()?;
            debug!("popping {:?}", value);
        }

        match entry.label {
            Label::Bound(target) => {
                debug!("Setting execution point from {} to {}", ip, target);
                Ok(target)
            }
            Label::Unbound => {
                debug!("Seeking for branch destination");

                let mut depth = num_entries;
                let mut offset = None;
                for (i, inst) in instructions[ip + 1 ..].iter().enumerate() {
                    debug!("  > {:?}", inst);
                    match inst {
                        Instruction::Block(_) | Instruction::Loop(_) => {
                            debug!("depth+");
                            depth += 1;
                        }
                        Instruction::End => {
                            if depth == 0 {
                                offset = Some(i);
                                break;
                            } else {
                                debug!("depth-");
                                depth -= 1;
                            }
                        }
                        _ => ()
                    }
                }

                if let Some(offset) = offset {
                    // TODO: cache the target with the instruction somehow.

                    let target = ip + offset;
                    debug!("Setting execution point from {} to {}", ip, target);
                    Ok(target)
                } else {
                    Err(Error::Runtime("scanning for branch point failed"))
                }
            }
        }
    }
}

impl ::std::fmt::Debug for ModuleEnvironment {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.write_str("ModuleEnvironment {\n")?;

        // Rather than delegating to FunctionDefinition's Debug impl here, iterate over and print
        // the code ourselves. This lets us try to use symbol table info to include names of things.
        for (i, fun) in self.functions.iter().enumerate() {
            let name = self.function_name(i).unwrap_or("?");
            f.write_fmt(format_args!("    Function {} ({}): {{\n", i, name))?;
            f.write_fmt(format_args!("        signature: {:?}\n", fun.signature))?;
            match fun.definition {
                FunctionDefinition::Internal { ref locals, ref instructions } => {
                    f.write_str("        definition: FunctionDefinition::Internal {\n")?;
                    f.write_fmt(format_args!("            locals: {:?}\n", locals))?;
                    f.write_fmt(format_args!("            instructions: ({}) [\n", instructions.len()))?;
                    for i in instructions {
                        match i {
                            Instruction::Call(index) => {
                                if let Some(name) = self.function_name(*index as usize) {
                                    f.write_fmt(format_args!("                Call({})\n", name))?;
                                } else {
                                    f.write_fmt(format_args!("                Call({})\n", index))?;
                                }
                            }
                            _ => f.write_fmt(format_args!("                {:?}\n", i))?
                        }
                    }
                    f.write_str("            ]\n")?;
                    f.write_str("        }\n")?;
                },
                FunctionDefinition::Import(_) => {
                    f.write_str("        definition: FunctionDefinition::Import(_)\n")?;
                }
            }
            f.write_str("    }\n")?;
        }

        f.write_fmt(format_args!("    start: {:?}\n", self.start))?;
        f.write_str("}\n")
    }
}

fn eval_initializer(code: &[Instruction], globals: &[GlobalEntry]) -> Result<Value, Error> {
    let value = match code {
        [Instruction::I32Const(n)] => Value::I32(*n),
        [Instruction::I64Const(n)] => Value::I64(*n),
        [Instruction::F32Const(n)] => Value::F32(*n),
        [Instruction::F64Const(n)] => Value::F64(*n),
        [Instruction::GetGlobal(idx)] => {
            globals.get(*idx as usize)
                .ok_or(Error::Instantiation("initializer from global is out of range"))?
                .value
        }
        _ => {
            return Err(Error::Instantiation("invalid code in initializer expression"));
        }
    };
    Ok(value)
}

// TODO: need to represent and pass in the external environment somehow
pub fn instantiate_module<R: io::Read>(r: R, mut host_env: HostEnvironment)
    -> Result<(ModuleEnvironment, MutableState), Error>
{
    let module = Module::read(r).map_err(|(e, offset)| match e {
        wasm_binary::Error::IO(e) => Error::IO(e),
        wasm_binary::Error::Invalid(e) => Error::BinaryFormat(e, offset),
    })?;

    if module.memory.len() > 1 {
        return Err(Error::Instantiation("only one memory section is allowed"));
    }

    let mut memory = vec![];
    for import in &module.imports {
        if let wasm_binary::module::ExternalType::Memory(ref mtype) = import.typ {
            debug!("import memory from {}.{} of size {:?}",
                import.module_name, import.field_name, mtype.limits);
            //memory.resize(mtype.limits.initial_len as usize * PAGE_SIZE, 0);
            return Err(Error::Instantiation("memory imports not yet implemented"));
        }
    }

    // TODO: make sure it doesn't import memory and also define its own memory.

    if let Some(mem_limits) = module.memory.get(0).map(|section| &section.limits) {
        debug!("memory {:?}", mem_limits);
        memory.resize(mem_limits.initial_len as usize * PAGE_SIZE, 0);
    }

    let mut globals = vec![];
    for (i, global) in module.globals.iter().enumerate() {
        debug!("global {}: {:?}", i, global);
        let inst = global.init.instructions()?;
        debug!("  init: {:?}", inst);
        let value = eval_initializer(&inst, &globals)?;
        if value.valuetype() != global.typ.content_type {
            return Err(Error::Instantiation("wrong type returned by initializer for global"));
        }
        debug!("  value: {:?}", value);
        globals.push(GlobalEntry {
            value,
            mutable: global.typ.mutable,
        });
    }

    for segment in &module.data {
        debug!("processing data segment");
        if segment.index != 0 {
            return Err(Error::Instantiation("data segment may not specify index > 0"));
        }

        let offset_code = segment.offset.instructions()?;

        let offset = match eval_initializer(&offset_code, &globals) {
            Ok(Value::I32(n)) => {
                if n >= 0 {
                    n as usize
                } else {
                    return Err(Error::Instantiation("data segment offset is negative"));
                }
            }
            Ok(_) => {
                return Err(Error::Instantiation(
                        "data segment offset initializer expression returns the wrong type"));
            }
            Err(e) => return Err(e)
        };

        debug!("{:?} -> {} bytes @ {:#x}", offset_code, segment.data.len(), offset);

        memory.as_mut_slice()[offset .. offset + segment.data.len()]
            .copy_from_slice(&segment.data);
    }

    let mut functions = vec![];
    for import in &module.imports {
        if let module::ExternalType::Function(type_idx) = import.typ {
            if import.module_name != "env" {
                return Err(Error::Instantiation(
                    "cross-module imports are not yet supported"));
            }
            if let Some(def) = host_env.functions.remove(&import.field_name) {
                let signature = module.types.get(type_idx)
                    .cloned()
                    .ok_or(Error::Instantiation(
                        "import type index out of bounds"))?;
                let f = Function {
                    signature,
                    definition: FunctionDefinition::Import(def),
                };
                functions.push(f);
            } else {
                // todo: make errors take a String so you know what
                return Err(Error::MissingImport {
                    module: "env".to_string(),
                    field: import.field_name.clone(),
                });
            }
        }
    }

    for (idx, body) in module.code.iter().enumerate() {
        let type_idx = module.functions.get(idx)
            .cloned()
            .ok_or(Error::Instantiation(
                "code and function table size mismatch"))?;
        let signature = module.types.get(type_idx)
            .cloned()
            .ok_or(Error::Instantiation(
                "function table type index out of bounds"))?;
        let instructions = instructions::read_instructions(&body.code)?;
        let f = Function {
            signature,
            definition: FunctionDefinition::Internal {
                locals: body.locals.clone(),
                instructions,
            },
        };
        functions.push(f);
    }

    // TODO: table instantiation
    // TODO: build exports map

    let mut exports = HashMap::new();
    for entry in module.exports {
        exports.insert(entry.field_name, (entry.kind, entry.index));
    }

    let symtab = module.custom_sections
        .iter()
        .find(|section| section.name == "name")
        .and_then(|section| wasm_binary::name_section::SymbolTable::from_bytes(
                &section.payload)
            .map_err(|e| {
                error!("unable to parse symbol table from name section: {:?}", e);
                e
            })
            .ok());

    Ok((
            ModuleEnvironment {
                types: module.types.clone(),
                functions,
                start: module.start,
                exports,
                symtab,
            },
            MutableState {
                memory,
                globals,
            }))
}
