use Error;
use std::io;
use num_traits::FromPrimitive;
use util::{read_string, read_varu1, read_varu32};

pub const MAGIC_COOKIE: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];
pub const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

pub trait Read: Sized {
    fn read<R: io::Read>(r: R) -> Result<Self, Error>;
}

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
    pub types: Vec<FuncType>,
    pub imports: Vec<ImportEntry>,
    pub functions: Vec<usize>,
    pub tables: Vec<TableType>,
    pub memory: Vec<MemoryType>,
    pub globals: Vec<GlobalEntry>,
    pub exports: Vec<ExportEntry>,
    pub start: Option<usize>,
    pub elements: Vec<ElemSegment>,
    pub code: Vec<FunctionBody>,
    pub data: Vec<DataSegment>,
    pub custom_sections: Vec<CustomSection>,
}

impl Read for Module {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        ModuleHeader::read(&mut r)?;
        let mut module = Module::default();

        let mut prev_section: Option<SectionType> = None;
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

            // Validate that sections are present only once, and in order, except for Custom, which
            // can be specified multiple times must must be after any others.
            if let Some(prev) = prev_section.take() {
                if prev == SectionType::Custom && section_type != SectionType::Custom {
                    return Err(Error::Invalid("non-custom section after custom section"));
                }
                else if section_type != SectionType::Custom && section_type <= prev {
                    return Err(Error::Invalid("section out of order or duplicated"));
                }
            }
            prev_section = Some(section_type);

            // limit the bound of the reader to the length of this section
            let len = read_varu32(&mut r)?;
            let mut section_reader = r.take(u64::from(len));

            module.read_section(section_type, &mut section_reader)?;

            if section_reader.limit() != 0 {
                return Err(Error::Invalid("section payload not fully consumed"));
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
            }
            SectionType::Import => {
                let count = read_varu32(&mut section_reader)?;
                self.imports.reserve(count as usize);
                for _ in 0 .. count {
                    let entry = ImportEntry::read(&mut section_reader)?;
                    self.imports.push(entry);
                }
            }
            SectionType::Function => {
                let count = read_varu32(&mut section_reader)?;
                self.functions.reserve(count as usize);
                for _ in 0 .. count {
                    let idx = read_varu32(&mut section_reader)? as usize;
                    if idx >= self.types.len() {
                        return Err(Error::Invalid("function index out of range"));
                    }
                    self.functions.push(idx);
                }
            }
            SectionType::Table => {
                let count = read_varu32(&mut section_reader)?;
                self.tables.reserve(count as usize);
                for _ in 0 .. count {
                    let table = TableType::read(&mut section_reader)?;
                    self.tables.push(table);
                }
            }
            SectionType::Memory => {
                let count = read_varu32(&mut section_reader)?;
                self.memory.reserve(count as usize);
                for _ in 0 .. count {
                    let memory = MemoryType::read(&mut section_reader)?;
                    self.memory.push(memory);
                }
            }
            SectionType::Global => {
                let count = read_varu32(&mut section_reader)?;
                self.globals.reserve(count as usize);
                for _ in 0 .. count {
                    let global = GlobalEntry::read(&mut section_reader)?;
                    self.globals.push(global);
                }
            }
            SectionType::Export => {
                let count = read_varu32(&mut section_reader)?;
                self.exports.reserve(count as usize);
                for _ in 0 .. count {
                    let export = ExportEntry::read(&mut section_reader)?;
                    self.exports.push(export);
                }
            }
            SectionType::Start => {
                let index = read_varu32(&mut section_reader)? as usize;
                if index >= self.functions.len() {
                    return Err(Error::Invalid("start index out of range"));
                }
                self.start = Some(index);
            }
            SectionType::Element => {
                let count = read_varu32(&mut section_reader)?;
                self.elements.reserve(count as usize);
                for _ in 0 .. count {
                    let element = ElemSegment::read(&mut section_reader)?;

                    // TODO: validate that the element index is in range
                    // must be <= number of imported tables plus table section definitions

                    // TODO: validate that indices are in range
                    // must be <= number of imported functions plus function section definitions
                    /*
                    for idx in &element.elements {
                        ...
                    }
                    */
                    self.elements.push(element);
                }
            }
            SectionType::Code => {
                let count = read_varu32(&mut section_reader)?;
                self.code.reserve(count as usize);
                for _ in 0 .. count {
                    let body = FunctionBody::read(&mut section_reader)?;
                    self.code.push(body);
                }
            }
            SectionType::Data => {
                let count = read_varu32(&mut section_reader)?;
                self.data.reserve(count as usize);
                for _ in 0 .. count {
                    let data = DataSegment::read(&mut section_reader)?;
                    self.data.push(data);
                }
            }
            SectionType::Custom => {
                let section = CustomSection::read(&mut section_reader)?;
                self.custom_sections.push(section);
            }
        }

        Ok(())
    }
}

#[derive(Primitive, Debug, Copy, Clone, PartialEq, PartialOrd)]
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
            param_types,
            return_type,
        })
    }
}

#[derive(Primitive, Debug)]
pub enum ExternalKind {
    Function = 0x00,
    Table = 0x01,
    Memory = 0x02,
    Global = 0x03,
}

impl Read for ExternalKind {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        Self::from_u8(buf[0]).ok_or(Error::Invalid("invalid external kind"))
    }
}

#[derive(Debug)]
pub enum ExternalKindAndType {
    Function(usize),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

impl Read for ExternalKindAndType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let kind = ExternalKind::read(&mut r)?;
        let ret = match kind {
            ExternalKind::Function => ExternalKindAndType::Function(read_varu32(&mut r)? as usize),
            ExternalKind::Table => ExternalKindAndType::Table(TableType::read(&mut r)?),
            ExternalKind::Memory => ExternalKindAndType::Memory(MemoryType::read(&mut r)?),
            ExternalKind::Global => ExternalKindAndType::Global(GlobalType::read(&mut r)?),
        };
        Ok(ret)
    }
}

#[derive(Debug)]
pub struct TableType {
    pub element_type: ElementType,
    pub limits: ResizableLimits,
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
pub struct MemoryType {
    pub limits: ResizableLimits,
}

impl Read for MemoryType {
    fn read<R: io::Read>(r: R) -> Result<Self, Error> {
        let limits = ResizableLimits::read(r)?;
        Ok(MemoryType {
            limits,
        })
    }
}

#[derive(Debug)]
pub struct ResizableLimits {
    pub initial_len: u32,
    pub maximum_len: Option<u32>,
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
    pub content_type: ValueType,
    pub mutable: bool,
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
    pub module_name: String,
    pub field_name: String,
    pub kind: ExternalKindAndType,
}

impl Read for ImportEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let module_name = read_string(&mut r)?;
        let field_name = read_string(&mut r)?;
        let kind = ExternalKindAndType::read(&mut r)?;
        Ok(ImportEntry {
            module_name,
            field_name,
            kind,
        })
    }
}

#[derive(Debug)]
pub struct GlobalEntry {
    pub typ: GlobalType,
    pub init: InitializerExpression,
}

impl Read for GlobalEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let typ = GlobalType::read(&mut r)?;
        let init = InitializerExpression::read(&mut r)?;
        Ok(GlobalEntry {
            typ,
            init,
        })
    }
}

#[derive(Debug)]
pub struct ExportEntry {
    pub field_name: String,
    pub kind: ExternalKind,
    pub index: usize,
}

impl Read for ExportEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let field_name = read_string(&mut r)?;
        let kind = ExternalKind::read(&mut r)?;
        // TODO check that the index is in bounds
        let index = read_varu32(&mut r)? as usize;
        Ok(ExportEntry {
            field_name,
            kind,
            index,
        })
    }
}

#[derive(Debug)]
pub struct ElemSegment {
    pub index: usize,
    pub offset: InitializerExpression,
    pub elements: Vec<usize>, // indices into Function table
}

impl Read for ElemSegment {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let index = read_varu32(&mut r)? as usize;
        let offset = InitializerExpression::read(&mut r)?;
        let num_elem = read_varu32(&mut r)? as usize;
        let mut elements = Vec::with_capacity(num_elem);
        for _ in 0 .. num_elem {
            let element = read_varu32(&mut r)? as usize;
            elements.push(element);
        }
        Ok(ElemSegment {
            index,
            offset,
            elements,
        })
    }
}

#[derive(Debug)]
pub struct FunctionBody {
    pub locals: Vec<LocalEntry>,
    pub code: Vec<u8>,
}

impl Read for FunctionBody {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        use io::Read;

        let body_size = read_varu32(&mut r)?;
        let mut r = r.take(u64::from(body_size));

        let local_count = read_varu32(&mut r)?;
        let mut locals = Vec::with_capacity(local_count as usize);
        for _ in 0 .. local_count {
            let local = LocalEntry::read(&mut r)?;
            locals.push(local);
        }

        let mut code = Vec::with_capacity(r.limit() as usize);
        r.read_to_end(&mut code).map_err(Error::IO)?;
        if code.last().cloned() != Some(0x0b) {
            return Err(Error::Invalid("code does not end with END opcode"));
        }

        Ok(FunctionBody {
            locals,
            code,
        })
    }
}

#[derive(Debug)]
pub struct LocalEntry {
    pub count: u32,
    pub typ: ValueType,
}

impl Read for LocalEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let count = read_varu32(&mut r)?;
        let typ = ValueType::read(&mut r)?;
        Ok(LocalEntry {
            count,
            typ,
        })
    }
}

#[derive(Debug)]
pub struct DataSegment {
    index: u32,
    offset: InitializerExpression,
    data: Vec<u8>,
}

impl Read for DataSegment {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        use io::Read;
        let index = read_varu32(&mut r)?;
        let offset = InitializerExpression::read(&mut r)?;
        let size = read_varu32(&mut r)?;
        let mut data = Vec::with_capacity(size as usize);
        r.take(u64::from(size)).read_to_end(&mut data).map_err(Error::IO)?;
        Ok(DataSegment {
            index,
            offset,
            data,
        })
    }
}

#[derive(Debug)]
pub struct CustomSection {
    pub name: String,
    pub payload: Vec<u8>,
}

impl Read for CustomSection {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let name = read_string(&mut r)?;
        let mut payload = vec![];
        r.read_to_end(&mut payload).map_err(Error::IO)?;
        Ok(CustomSection {
            name,
            payload,
        })
    }
}

#[derive(Debug)]
pub struct InitializerExpression {
    pub bytes: Vec<u8>, // TODO
}

impl Read for InitializerExpression {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut bytes = vec![];
        let mut buf = [0u8];
        loop {
            r.read_exact(&mut buf).map_err(Error::IO)?;
            bytes.push(buf[0]);
            if buf[0] == 0x0B { // END opcode
                return Ok(InitializerExpression {
                    bytes,
                });
            }
        }
    }
}
