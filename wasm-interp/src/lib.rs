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

    pub fn call_function(&mut self, name: &str, args: &[Value], memory: &mut Vec<u8>)
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

        self.call(idx, &mut stack, memory)?;

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

    fn call(&self, idx: usize, stack: &mut Stack, memory: &mut Vec<u8>) -> Result<(), Error> {
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
                    let val = stack.pop()?;
                    debug!("adding param to locals: {:?}", val);
                    if val.valuetype() != *param_type {
                        return Err(Error::Runtime("wrong value type passed as parameter"));
                    }

                    debug!("local var name {:?}",
                        self.local_name(idx, stack.len()).unwrap_or("<unnamed>"));

                    locals.push(val);
                }

                for local in local_specs {
                    for _ in 0 .. local.count {
                        debug!("adding empty {:?} value to locals", local.typ);
                        locals.push(Self::zero_value(local.typ));
                    }
                }

                let mut control = vec![ControlEntry {
                    label: Label::Bound(instructions.len()),
                    stack_limit: 0,
                    signature: module::BlockType::from(f.signature.return_type)
                }];

                self.call_internal(instructions, stack, memory, &mut control, &mut locals)?;
            }
            FunctionDefinition::Import(lambda) => {
                info!("imported function, {:?}", f.signature);
                Self::call_import(&f.signature, lambda, stack, memory)?;
            }
        }

        Ok(())
    }

    fn call_internal(
        &self,
        instructions: &[Instruction],
        stack: &mut Stack,
        memory: &mut Vec<u8>,
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
                    debug!("block returning {:?}", return_type);
                    control.push(ControlEntry {
                        label: Label::Unbound,
                        stack_limit: stack.len(),
                        signature: *return_type,
                    });
                }
                Instruction::Loop(return_type) => {
                    debug!("loop returning {:?}", return_type);
                    control.push(ControlEntry {
                        label: Label::Bound(ip),
                        stack_limit: stack.len(),
                        signature: *return_type,
                    });
                }
                Instruction::If(return_type) => {
                    debug!("if block returning {:?}", return_type);
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
                    let cond = popt!(stack, Value::I32)? == 1;
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
                    self.call(*idx as usize, stack, memory)?;
                }
                Instruction::CallIndirect(type_idx) => {
                    // pop, interpret as fn index, compare signatures
                    let signature = self.types.get(*type_idx as usize)
                        .ok_or(Error::Runtime("type index out of bounds for indirect call"))?;
                    debug!("CallIndirect with type signature {:?}", signature);
                    //TODO
                    unimplemented!();
                }
                Instruction::Drop => {
                    let dropped = stack.pop();
                    debug!("Drop: {:?}", dropped);
                    dropped?;
                }
                Instruction::Select => {
                    let a = stack.pop()?;
                    let b = stack.pop()?;
                    let cond = stack.pop()?;
                    debug!("Select: {:?} {:?} {:?}", a, b, cond);
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

                // TODO: more instructions

                Instruction::I32Load(arg) => {
                    let mut value = 0u32;
                    // TODO: what about the alignment?
                    let offset = arg.offset as usize;
                    for i in 0 .. 4 {
                        value |= (memory[offset + i] as u32) << (8 * i);
                    }
                    debug!("loaded {} from {:#x}", value as i32, offset);
                    stack.push(Value::I32(value as i32));
                }

                // ...

                Instruction::I32Store(arg) => {
                    let value = popt!(stack, Value::I32)?;
                    let offset = arg.offset as usize;
                    debug!("storing {} to {:#x}", value, offset);
                    for i in 0 .. 4 {
                        memory[offset + i] = ((value & (0xFF << (8 * i))) >> (8 * i)) as u8;
                    }
                }

                // ...

                Instruction::I32Const(value) => {
                    stack.push(Value::I32(*value));
                }

                // ...

                Instruction::I32Eqz => {
                    let x = popt!(stack, Value::I32)?;
                    debug!("{} == 0 = {:?}", x, x == 0);
                    stack.push(Value::I32(if x == 0 { 1 } else { 0 }));
                }

                Instruction::I32Eq => {
                    let a = popt!(stack, Value::I32)?;
                    let b = popt!(stack, Value::I32)?;
                    debug!("{} == {} = {:?}", a, b, a == b);
                    stack.push(Value::I32(if a == b { 1 } else { 0 }));
                }

                // ...

                Instruction::I32LtS => {
                    let a: i32 = popt!(stack, Value::I32)?;
                    let b = popt!(stack, Value::I32)?;
                    debug!("{} < {} = {:?}", a, b, a < b);
                    stack.push(Value::I32(if a < b { 1 } else { 0 }));
                }

                // ...

                Instruction::I32Add => {
                    match (stack.pop()?, stack.pop()?) {
                        (Value::I32(a), Value::I32(b)) => {
                            debug!("I32Add: {} + {} -> {}", a, b, a + b);
                            stack.push(Value::I32(a + b));
                        }
                        _ => {
                            return Err(Error::Runtime("argument wrong type for I32Add"));
                        }
                    }
                }
                Instruction::I32Sub => {
                    let a = popt!(stack, Value::I32)?;
                    let b = popt!(stack, Value::I32)?;
                    let c = a - b;
                    debug!("{} - {} = {}", a, b, c);
                    stack.push(Value::I32(c));
                }

                // ...

                Instruction::I32And => {
                    let a = popt!(stack, Value::I32)?;
                    let b = popt!(stack, Value::I32)?;
                    let c = a & b;
                    debug!("{} & {} = {}", a, b, c);
                    stack.push(Value::I32(c));
                }
                Instruction::I32Xor => {
                    let a = popt!(stack, Value::I32)?;
                    let b = popt!(stack, Value::I32)?;
                    let c = a ^ b;
                    debug!("{} ^ {} = {}", a, b, c);
                    stack.push(Value::I32(c));
                }

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
        memory: &mut Vec<u8>,
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

        let entry = control.pop()
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

                let mut depth = 0;
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

                    let target = ip + offset + 1;
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

// TODO: need to represent and pass in the external environment somehow
pub fn instantiate_module<R: io::Read>(r: R, mut host_env: HostEnvironment)
    -> Result<(ModuleEnvironment, Vec<u8>), Error>
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

    for segment in &module.data {
        debug!("processing data segment");
        if segment.index != 0 {
            return Err(Error::Instantiation("data segment may not specify index > 0"));
        }

        let offset_code = segment.offset.instructions()?;

        let offset = match offset_code[..] {
            // The spec says this needs to evaluate to an i32, so...
            [Instruction::I32Const(offset)] => {
                if offset < 0 {
                    return Err(Error::Instantiation("data segment offset is negative"));
                }
                offset as usize
            }
            [Instruction::I64Const(_)]
                | [Instruction::F32Const(_)]
                | [Instruction::F64Const(_)] =>
            {
                return Err(Error::Instantiation(
                    "data segment offset initializer expression returns the wrong type"));
            }
            [Instruction::GetGlobal(_index)] => {
                return Err(Error::Instantiation(
                    "loading data segment offsets from imports isn't supported yet"));
            }
            _ => {
                return Err(Error::Instantiation(
                    "invalid code in data segment offset initializer expression"));
            }
        };

        debug!("{:?} -> {}, {}", offset_code, offset, segment.data.len());

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
    // TODO: global instantiation
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
            memory))
}
