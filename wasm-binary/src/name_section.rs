//! Structures representing the "name" custom section, which provides debugging info in the wasm
//! file.

use Error;
use module::Read;
use util::{read_varu32, read_string};
use std::io;
use std::collections::btree_map::*;
use num_traits::FromPrimitive;

/// Represents the information in the "name" custom section as a mapping.
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// The name of the module, if present.
    pub module_name: Option<String>,

    /// A mapping from function index to the name of the function and then a mapping from local
    /// variable index to their name. Either the function name or the locals map may not be present.
    pub functions: Option<BTreeMap<usize, SymbolTableEntry>>,
}

impl SymbolTable {
    /// Read the symbol table from the bytes of the payload in the "name" custom section.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, Error> {
        let section = NameSection::read(bytes)?;
        Self::from_section(section)
    }

    /// Read the symbol table from an already parsed-out `NameSection` struct.
    pub fn from_section(section: NameSection) -> Result<Self, Error> {
        let mut table = Self::default();
        for subsection in section.subsections {
            match subsection {
                NameSubsection::ModuleName(name) => {
                    table.module_name = Some(name);
                },
                NameSubsection::FunctionNames(NameMap(names)) => {
                    let mut map = BTreeMap::new();
                    for entry in names {
                        map.insert(entry.index, SymbolTableEntry {
                            name: Some(entry.name),
                            locals: None,
                        });
                    }
                    table.functions = Some(map);
                },
                NameSubsection::LocalNames(section_entries) => {
                    if table.functions.is_none() {
                        table.functions = Some(BTreeMap::new());
                    }
                    let funcs = table.functions.as_mut().unwrap();

                    for section_func_entry in section_entries {
                        let mut local_map = BTreeMap::new();
                        for local_entry in section_func_entry.local_map.0 {
                            local_map.insert(local_entry.index, local_entry.name);
                        }

                        match funcs.entry(section_func_entry.function_index) {
                            Entry::Occupied(mut entry) => {
                                entry.get_mut().locals = Some(local_map);
                            },
                            Entry::Vacant(entry) => {
                                entry.insert(SymbolTableEntry {
                                    name: None,
                                    locals: Some(local_map),
                                });
                            }
                        }
                    }
                }
            }
        }
        Ok(table)
    }
}

/// An entry in the symbol table for a single function.
/// At least one of `name` or `locals` will not be `None`.
#[derive(Debug)]
pub struct SymbolTableEntry {
    /// The name of the function, if present.
    pub name: Option<String>,

    /// A mapping from local variable index to variable name, if locals info is present.
    pub locals: Option<BTreeMap<usize, String>>,
}

// Below here are lower-level structs that more closely match the format that's actually in the
// binary.

#[derive(Debug)]
pub struct NameSection {
    pub subsections: Vec<NameSubsection>,
}

impl NameSection {
    // Note: this can't implement module::Read because it needs to know the length of the section.
    pub fn read(bytes: &[u8]) -> Result<Self, Error> {
        let mut r = io::Cursor::new(bytes);
        let mut subsections = Vec::new();
        while r.position() < bytes.len() as u64 {
            let subsection = NameSubsection::read(&mut r)?;
            subsections.push(subsection);
        }
        Ok(NameSection {
            subsections,
        })
    }
}

#[derive(Primitive, Debug)]
pub enum NameType {
    Module = 0x00,
    Function = 0x01,
    Local = 0x02,
}

impl Read for NameType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        Self::from_u8(buf[0]).ok_or(Error::Invalid("invalid name type"))
    }
}

#[derive(Debug)]
pub enum NameSubsection {
    ModuleName(String),
    FunctionNames(NameMap),
    LocalNames(Vec<LocalNames>),
}

impl Read for NameSubsection {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let typ = NameType::read(&mut r)?;
        let payload_len = read_varu32(&mut r)?;
        let mut r = r.take(payload_len as u64);
        match typ {
            NameType::Module => {
                let name = read_string(r)?;
                Ok(NameSubsection::ModuleName(name))
            }
            NameType::Function => {
                let map = NameMap::read(r)?;
                Ok(NameSubsection::FunctionNames(map))
            }
            NameType::Local => {
                let count = read_varu32(&mut r)?;
                let mut names = Vec::with_capacity(count as usize);
                for _ in 0 .. count {
                    let local_names = LocalNames::read(&mut r)?;
                    names.push(local_names);
                }
                Ok(NameSubsection::LocalNames(names))
            }
        }
    }
}

#[derive(Debug)]
pub struct LocalNames {
    pub function_index: usize,
    pub local_map: NameMap,
}

impl Read for LocalNames {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let function_index = read_varu32(&mut r)? as usize;
        let local_map = NameMap::read(&mut r)?;
        Ok(LocalNames {
            function_index,
            local_map,
        })
    }
}

#[derive(Debug)]
pub struct NameMap(Vec<Naming>);

impl Read for NameMap {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let count = read_varu32(&mut r)?;
        let mut names = Vec::with_capacity(count as usize);
        for _ in 0 .. count {
            let naming = Naming::read(&mut r)?;
            names.push(naming);
        }
        Ok(NameMap(names))
    }
}

#[derive(Debug)]
pub struct Naming {
    pub index: usize,
    pub name: String,
}

impl Read for Naming {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let index = read_varu32(&mut r)? as usize;
        let name = read_string(&mut r)?;
        Ok(Naming {
            index,
            name,
        })
    }
}
