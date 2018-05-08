use std::io;
use leb128;
use num_traits::FromPrimitive;

// used as an escape hatch for debugging
const UNIMPLEMENTED: &str = "UNIMPLEMENTED";

#[derive(Debug)]
pub enum Error {
    Invalid(&'static str),
    IO(io::Error),
}

pub trait Read: Sized {
    fn read<R: io::Read>(r: R) -> Result<Self, Error>;
}

pub fn read_varu32<R: io::Read>(r: R) -> Result<u32, Error> {
    let mut r2 = r.take(4);
    match leb128::read::unsigned(&mut r2) {
        Ok(n) => Ok(n as u32),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varuint32"))
    }
}

fn read_varu1<R: io::Read>(r: R) -> Result<bool, Error> {
    match read_varu32(r.take(1)) {
        Ok(0) => Ok(false),
        Ok(1) => Ok(true),
        Ok(_) => Err(Error::Invalid("varuint1 out of range")),
        Err(e) => Err(e),
    }
}

fn read_string<R: io::Read>(mut r: R) -> Result<String, Error> {
    use std::io::Read;
    let len = read_varu32(&mut r)?;
    let mut s = String::new();
    r.take(u64::from(len)).read_to_string(&mut s)
        .map_err(Error::IO)?;
    Ok(s)
}

pub const MAGIC_COOKIE: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];
pub const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

#[derive(Debug)]
pub struct ModuleHeader;

impl Read for ModuleHeader {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8; 4];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        if buf != MAGIC_COOKIE {
            return Err(Error::Invalid("invalid magic cookie in module header"));
        }

        r.read_exact(&mut buf).map_err(Error::IO)?;
        if buf != VERSION {
            return Err(Error::Invalid("unsupported version in module header"));
        }

        Ok(ModuleHeader)
    }
}

#[derive(Debug, Default)]
pub struct Module {
    types: Vec<FuncType>,
    imports: Vec<ImportEntry>,
    functions: Vec<usize>,
    table: Vec<TableType>,
    memory: Vec<ResizableLimits>,
    globals: Vec<GlobalEntry>,
    exports: Vec<ExportEntry>,
    start: Option<usize>,
    elements: Vec<ElemSegment>,
    code: Vec<FunctionBody>,
    data: Vec<DataSegment>,
    custom_sections: Vec<CustomSection>,
}

impl Read for Module {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        ModuleHeader::read(&mut r)?;
        let mut module = Module::default();

        loop {
            // don't use SectionHeader::read here, because we need to know if we're exactly at the
            // end of the input.

            let section_type = match SectionType::read(&mut r) {
                Ok(t) => t,
                Err(Error::IO(ref e)) if e.kind() == io::ErrorKind::UnexpectedEof => {
                    // if we hit EOF on reading the first byte of the section, it means we're done.
                    break
                },
                Err(e) => { return Err(e) },
            };
            let len = read_varu32(&mut r)?;

            // limit the bound of the reader for the duration of this section
            let mut section_reader = r.take(u64::from(len));

            match module.read_section(section_type, &mut section_reader) {
                Err(Error::Invalid(UNIMPLEMENTED)) => break,
                other => other?,
            }

            // restore the reader to one without limits
            r = section_reader.into_inner();
        }

        Ok(module)
    }
}

impl Module {
    fn read_section<R: io::Read>(&mut self, section_type: SectionType, mut section_reader: R)
        -> Result<(), Error>
    {
        match section_type {
            SectionType::Type => {
                let count = read_varu32(&mut section_reader)?;
                self.types.reserve(count as usize);
                for _ in 0 .. count {
                    let t = FuncType::read(&mut section_reader)?;
                    self.types.push(t);
                }
            },
            SectionType::Import => {
                let count = read_varu32(&mut section_reader)?;
                self.imports.reserve(count as usize);
                for _ in 0 .. count {
                    let entry = ImportEntry::read(&mut section_reader)?;
                    self.imports.push(entry);
                }
            }
            _ => return Err(Error::Invalid(UNIMPLEMENTED)), // FIXME: for testing purposes only
            //_ => unimplemented!("unimplemented section {:?}", typ)
        }

        Ok(())
    }
}

#[derive(Primitive, Debug)]
pub enum SectionType {
    Custom = 0x00,
    Type = 0x01,
    Import = 0x02,
    Function = 0x03,
    Table = 0x04,
    Memory = 0x05,
    Global = 0x06,
    Export = 0x07,
    Start = 0x08,
    Element = 0x09,
    Code = 0x0a,
    Data = 0x0b,
}

impl Read for SectionType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        Self::from_u8(buf[0]).ok_or(Error::Invalid("invalid section type"))
    }
}

#[derive(Debug)]
pub struct SectionHeader {
    pub typ: SectionType,
    pub size: u32,
}

impl Read for SectionHeader {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        Ok(SectionHeader {
            typ: SectionType::read(r.by_ref())?,
            size: read_varu32(r)?,
        })
    }
}

#[derive(Primitive, Debug, PartialEq)]
pub enum Type {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
    Anyfunc = 0x70,
    Func = 0x60,
    Void = 0x40,
}

impl Read for Type {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        Self::from_u8(buf[0]).ok_or(Error::Invalid("invalid type"))
    }
}

#[derive(Debug)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

impl Read for ValueType {
    fn read<R: io::Read>(r: R) -> Result<Self, Error> {
        Ok(match Type::read(r)? {
            Type::I32 => ValueType::I32,
            Type::I64 => ValueType::I64,
            Type::F32 => ValueType::F32,
            Type::F64 => ValueType::F64,
            _ => { return Err(Error::Invalid("invalid value type")); }
        })
    }
}

#[derive(Debug)]
pub enum BlockType {
    I32,
    I64,
    F32,
    F64,
    Void,
}

impl Read for BlockType {
    fn read<R: io::Read>(r: R) -> Result<Self, Error> {
        Ok(match Type::read(r)? {
            Type::I32 => BlockType::I32,
            Type::I64 => BlockType::I64,
            Type::F32 => BlockType::F32,
            Type::F64 => BlockType::F64,
            Type::Void => BlockType::Void,
            _ => { return Err(Error::Invalid("invalid block type")); }
        })
    }
}

#[derive(Debug)]
pub enum ElementType {
    Anyfunc,
}

impl Read for ElementType {
    fn read<R: io::Read>(r: R) -> Result<Self, Error> {
        Ok(match Type::read(r)? {
            Type::Anyfunc => ElementType::Anyfunc,
            _ => { return Err(Error::Invalid("invalid element type")); }
        })
    }
}

#[derive(Debug)]
pub struct FuncType {
    pub form: Type,
    pub param_count: u32,
    pub param_types: Vec<ValueType>,
    pub return_type: Option<ValueType>,
}

impl Read for FuncType {
    fn read<R: io::Read>(mut r: R) -> Result<FuncType, Error> {
        let typ = Type::read(&mut r)?;
        if typ != Type::Func {
            return Err(Error::Invalid("form must be type 'func' in func_type"));
        }

        let param_count = read_varu32(&mut r)?;
        let mut param_types = Vec::with_capacity(param_count as usize);
        for _ in 0 .. param_count {
            param_types.push(ValueType::read(&mut r)?);
        }

        let return_type = if read_varu1(&mut r)? {
            Some(ValueType::read(r)?)
        } else {
            None
        };

        Ok(FuncType {
            form: typ,
            param_count,
            param_types,
            return_type,
        })
    }
}

#[derive(Debug)]
pub enum ExternalKind {
    Function(usize),
    Table(TableType),
    Memory(ResizableLimits),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct TableType {
    element_type: ElementType,
    limits: ResizableLimits,
}

impl Read for TableType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let element_type = ElementType::read(&mut r)?;
        let limits = ResizableLimits::read(&mut r)?;
        Ok(TableType {
            element_type,
            limits,
        })
    }
}

#[derive(Debug)]
pub struct ResizableLimits {
    initial_len: u32,
    maximum_len: Option<u32>,
}

impl Read for ResizableLimits {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let flags = read_varu1(&mut r)?;
        let initial_len = read_varu32(&mut r)?;
        let maximum_len = if flags {
            Some(read_varu32(&mut r)?)
        } else {
            None
        };
        Ok(ResizableLimits {
            initial_len,
            maximum_len,
        })
    }
}

#[derive(Debug)]
pub struct GlobalType {
    content_type: ValueType,
    mutable: bool,
}

impl Read for GlobalType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let content_type = ValueType::read(&mut r)?;
        let mutable = read_varu1(&mut r)?;
        Ok(GlobalType {
            content_type,
            mutable,
        })
    }
}

#[derive(Debug)]
pub struct ImportEntry {
    module_name: String,
    field_name: String,
    kind: ExternalKind,
}

impl Read for ImportEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let module_name = read_string(&mut r)?;
        let field_name = read_string(&mut r)?;
        let mut kind_byte = [0u8];
        r.read_exact(&mut kind_byte).map_err(Error::IO)?;
        let kind = match kind_byte[0] {
            0 => ExternalKind::Function(read_varu32(&mut r)? as usize),
            1 => ExternalKind::Table(TableType::read(&mut r)?),
            2 => ExternalKind::Memory(ResizableLimits::read(&mut r)?),
            3 => ExternalKind::Global(GlobalType::read(&mut r)?),
            _ => { return Err(Error::Invalid("unrecognized import entry kind")); }
        };
        Ok(ImportEntry {
            module_name,
            field_name,
            kind,
        })
    }
}

#[derive(Debug)]
pub struct GlobalEntry;

#[derive(Debug)]
pub struct ExportEntry;

#[derive(Debug)]
pub struct ElemSegment;

#[derive(Debug)]
pub struct FunctionBody;

#[derive(Debug)]
pub struct DataSegment;

#[derive(Debug)]
pub struct CustomSection {
    name: String,
    paylod: Vec<u8>,
}
