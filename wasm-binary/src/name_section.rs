use Error;
use module::Read;
use util::{read_varu32, read_string};
use std::io;
use num_traits::FromPrimitive;

#[derive(Debug)]
pub struct NameSection {
    pub subsections: Vec<NameSubsection>,
}

impl NameSection {
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
