#[macro_use] extern crate log;

extern crate wasm_binary;
use wasm_binary::Module;
use wasm_binary::instructions::Instruction;

use std::io;

const PAGE_SIZE: usize = 64 * 1024;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    BinaryFormat(&'static str, u64),
    ModuleValidation(&'static str),
}

impl From<wasm_binary::Error> for Error {
    fn from(other: wasm_binary::Error) -> Error {
        match other {
            wasm_binary::Error::IO(e) => Error::IO(e),
            wasm_binary::Error::Invalid(e) => Error::BinaryFormat(e, 0),
        }
    }
}

pub struct ModuleEnvironment {
    module: Module,
    memory: Vec<u8>,
}

// TODO: need to represent and pass in the external environment somehow
pub fn instantiate_module<R: io::Read>(r: R) -> Result<ModuleEnvironment, Error> {
    let module = Module::read(r).map_err(|(e, offset)| match e {
        wasm_binary::Error::IO(e) => Error::IO(e),
        wasm_binary::Error::Invalid(e) => Error::BinaryFormat(e, offset),
    })?;

    if module.memory.len() > 1 {
        return Err(Error::ModuleValidation("only one memory section is allowed"));
    }

    let mut memory = vec![];
    for import in &module.imports {
        if let wasm_binary::module::ExternalType::Memory(ref mtype) = import.typ {
            debug!("import memory from {}.{} of size {:?}",
                import.module_name, import.field_name, mtype.limits);
            //memory.resize(mtype.limits.initial_len as usize * PAGE_SIZE, 0);
            return Err(Error::ModuleValidation("memory imports not yet implemented"));
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
            return Err(Error::ModuleValidation("data segment may not specify index > 0"));
        }

        let offset_code = segment.offset.instructions()?;

        let offset = match &offset_code[..] {
            // The spec says this needs to evaluate to an i32, so...
            &[Instruction::I32Const(offset)] => {
                if offset < 0 {
                    return Err(Error::ModuleValidation("data segment offset is negative"));
                }
                offset as usize
            }
            &[Instruction::I64Const(_)]
                | &[Instruction::F32Const(_)]
                | &[Instruction::F64Const(_)] =>
            {
                return Err(Error::ModuleValidation(
                    "data segment offset initializer expression returns the wrong type"));
            }
            &[Instruction::GetGlobal(_index)] => {
                return Err(Error::ModuleValidation(
                    "loading data segment offsets from imports isn't supported yet"));
            }
            _ => {
                return Err(Error::ModuleValidation(
                    "invalid code in data segment offset initializer expression"));
            }
        };

        debug!("{:?} -> {}, {}", offset_code, offset, segment.data.len());

        memory.as_mut_slice()[offset .. offset + segment.data.len()]
            .copy_from_slice(&segment.data);
    }

    // TODO: table instantiation

    Ok(ModuleEnvironment {
        module,
        memory,
    })
}
