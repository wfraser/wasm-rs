#[macro_use] extern crate log;

extern crate wasm_binary;
use wasm_binary::Module;
use wasm_binary::module;
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

/// Helper function to aid in making the right boxed function type, because rustc's type inference
/// makes it awkward otherwise.
pub fn make_import_function<F: 'static + Fn(Vec<Value>) -> Option<Value>>(f: F)
    -> ImportFunction
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

pub struct ModuleEnvironment {
    memory: Vec<u8>,
    functions: Vec<Function>,
    start: Option<usize>,
    symtab: Option<wasm_binary::name_section::SymbolTable>,
}

impl ModuleEnvironment {
    fn function_name(&self, index: usize) -> Option<&str> {
        self.symtab.as_ref()
            .and_then(|table| table.functions.as_ref())
            .and_then(|funcs| funcs.get(&index))
            .and_then(|func| func.name.as_ref())
            .map(|s| s.as_str())
    }
}

impl ::std::fmt::Debug for ModuleEnvironment {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.write_str("ModuleEnvironment {\n")?;

        f.write_str("    memory: {")?;
        /*
        for (i, byte) in self.memory.iter().enumerate() {
            if i % 16 == 0 {
                f.write_str("\n        ")?;
            } else if i % 8 == 0 {
                f.write_str(" ")?;
            }
            f.write_fmt(format_args!("{:02x} ", byte))?;
        }
        */
        f.write_fmt(format_args!("\n    <skipped {} bytes>", self.memory.len()))?;
        f.write_str("\n    }\n")?;

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
    -> Result<ModuleEnvironment, Error>
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

    Ok(ModuleEnvironment {
        memory,
        functions,
        start: module.start,
        symtab,
    })
}
