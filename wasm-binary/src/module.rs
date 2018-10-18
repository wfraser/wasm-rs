//! Structures representing the innards of a WASM module.

use Error;
use instructions::Opcode;
use std::io;
use num_traits::FromPrimitive;
use instructions::Instruction;
use util::{read_string, read_varu1, read_varu32};

/// The magic cookie present at the start of all WASM files. Looks like `"\0ASM"`.
pub const MAGIC_COOKIE: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

/// The version field in the WASM header. Currently set at `1u32`.
pub const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

/// A trait that a type can implement to allow reading it from any arbitrary `Read` impl and
/// returning appropriate errors.
pub trait Read: Sized {
    fn read<R: io::Read>(r: R) -> Result<Self, Error>;
}

/// Represents a WASM module and all its sections.
///
/// This struct does not parse the contents of any code sequences or custom sections, but keeps
/// their data as byte vectors that can be interpreted later. See the `instructions` and
/// `name_section` modules for doing that.
///
/// Note that a number of things in this structure include "index" fields. These are references to
/// things defined elsewhere in the module -- functions, globals, memory, tables, and types -- and
/// sometimes which one is referred to depends on context. The lists referred to also include
/// imports in addition to ones defined in the module itself. The order always is the order of
/// definition in the module, with entries in `imports` coming first, then internally-defined ones
/// from the appropriate field.
#[derive(Debug, Default)]
pub struct Module {
    /// The types the module defines.
    pub types: Vec<FuncType>,

    /// Items the module wants to import from its environment.
    pub imports: Vec<ImportEntry>,

    /// Type signatures of functions the module defines internally. The number is an index into the
    /// `types` list.
    pub functions: Vec<usize>,

    /// Tables defined in the module.
    pub tables: Vec<TableType>,

    /// Memory defined by the module.
    pub memory: Vec<MemoryType>,

    /// Global variables defined by the module, including their types and how to initialize them.
    pub globals: Vec<GlobalEntry>,

    /// Things the module exports to the environment.
    pub exports: Vec<ExportEntry>,

    /// The index of the start function of the module, if any.
    pub start: Option<usize>,

    /// Elements defined by the module, which allow for initializing imported or internally defined
    /// tables.
    pub elements: Vec<ElemSegment>,

    /// Function bodies for each function defined by the module.
    pub code: Vec<FunctionBody>,

    /// Data segments to be loaded into the module's memory at instantiation time.
    pub data: Vec<DataSegment>,

    /// Any number of "custom" sections, used for extending the WASM format. They are identified by
    /// name.
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

/// The module header. Contains no information other than the constant magic numbers.
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

/// The set of types currently supported by WASM.
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

/// Types that a value can be.
#[derive(Debug, Copy, Clone, PartialEq)]
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

/// Types that a block may return.
#[derive(Debug, PartialEq, Copy, Clone)]
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

impl From<Option<ValueType>> for BlockType {
    fn from(t: Option<ValueType>) -> BlockType {
        match t {
            Some(ValueType::I32) => BlockType::I32,
            Some(ValueType::I64) => BlockType::I64,
            Some(ValueType::F32) => BlockType::F32,
            Some(ValueType::F64) => BlockType::F64,
            None => BlockType::Void,
        }
    }
}

/// Types that an element may be.
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

/// Represents a function signature: its parameter count and types, and return type.
#[derive(Debug, Clone)]
pub struct FuncType {
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
            param_types,
            return_type,
        })
    }
}

#[derive(Primitive, Debug, PartialEq, Copy, Clone)]
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

/// The type of an external thing, used in imports and exports.
#[derive(Debug)]
pub enum ExternalType {
    /// References a function type defined in the `functions` section of this module.
    Function(usize),

    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

impl Read for ExternalType {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let kind = ExternalKind::read(&mut r)?;
        let ret = match kind {
            ExternalKind::Function => ExternalType::Function(read_varu32(&mut r)? as usize),
            ExternalKind::Table => ExternalType::Table(TableType::read(&mut r)?),
            ExternalKind::Memory => ExternalType::Memory(MemoryType::read(&mut r)?),
            ExternalKind::Global => ExternalType::Global(GlobalType::read(&mut r)?),
        };
        Ok(ret)
    }
}

#[derive(Debug)]
pub struct TableType {
    /// The type of the table
    pub element_type: ElementType,

    /// Initial and max size of the table.
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

/// Defines a memory area, including its initial and maximum size.
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

    /// Maximum size, if any. If `None`, it may grow unbounded.
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
    pub typ: ExternalType,
}

impl Read for ImportEntry {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let module_name = read_string(&mut r)?;
        let field_name = read_string(&mut r)?;
        let typ = ExternalType::read(&mut r)?;
        Ok(ImportEntry {
            module_name,
            field_name,
            typ,
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
        if code.last().cloned() != Some(Opcode::End as u8) {
            return Err(Error::Invalid("code does not end with END opcode"));
        }

        Ok(FunctionBody {
            locals,
            code,
        })
    }
}

impl ::std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.write_str("FunctionBody {\n")?;
        f.write_fmt(format_args!("    locals: {:?}\n", self.locals))?;
        f.write_str("    code: {")?;
        for (i, byte) in self.code.iter().enumerate() {
            if i % 16 == 0 {
                f.write_str("\n        ")?;
            } else if i % 8 == 0 {
                f.write_str(" ")?;
            }
            f.write_fmt(format_args!("{:02x} ", byte))?;
        }
        f.write_str("\n    }\n}")
    }
}

#[derive(Debug, Clone)]
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

pub struct DataSegment {
    pub index: u32,
    pub offset: InitializerExpression,
    pub data: Vec<u8>,
}

impl Read for DataSegment {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        use io::Read;
        let index = read_varu32(&mut r)?;
        if index != 0 {
            return Err(Error::Invalid("data segment index out of range (only 0 is supported)"));
        }
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

impl ::std::fmt::Debug for DataSegment {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.write_str("DataSegment {\n")?;
        f.write_fmt(format_args!("    index: {}\n", self.index))?;
        f.write_fmt(format_args!("    offset: {:?}\n", self.offset))?;
        f.write_str("    data: {")?;
        for (i, byte) in self.data.iter().enumerate() {
            if i % 16 == 0 {
                f.write_str("\n        ")?;
            } else if i % 8 == 0 {
                f.write_str(" ")?;
            }
            f.write_fmt(format_args!("{:02x} ", byte))?;
        }
        f.write_str("\n    }\n}")
    }
}

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

impl ::std::fmt::Debug for CustomSection {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.write_fmt(format_args!("CustomSection {:?} {{", self.name))?;
        for (i, byte) in self.payload.iter().enumerate() {
            if i % 16 == 0 {
                f.write_str("\n    ")?;
            } else if i % 8 == 0 {
                f.write_str(" ")?;
            }
            f.write_fmt(format_args!("{:02x} ", byte))?;
        }
        f.write_str("\n}")
    }
}

#[derive(Debug)]
pub struct InitializerExpression {
    pub bytes: Vec<u8>,
}

impl Read for InitializerExpression {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut bytes = vec![];
        let mut buf = [0u8];
        loop {
            r.read_exact(&mut buf).map_err(Error::IO)?;
            bytes.push(buf[0]);
            if buf[0] == Opcode::End as u8 {
                return Ok(InitializerExpression {
                    bytes,
                });
            }
        }
    }
}

impl InitializerExpression {
    /// Parse the initializer expression bytes into a sequence of instructions. This function does
    /// not include the terminating `End` instruction.
    ///
    /// Note that while it is currently only allowed to have 2 instructions -- a constant or a
    /// global load, followed by `End` -- future versions of WASM may allow arithmetic as well,
    /// which is why this returns a Vec.
    pub fn instructions(&self) -> Result<Vec<Instruction>, Error> {
        let mut instructions = ::instructions::read_instructions(&self.bytes)?;
        if instructions.pop() != Some(Instruction::End) {
            return Err(Error::Invalid("initializer expression must end with End"));
        }

        if instructions.len() != 1 {
            return Err(Error::Invalid("initializer expression must be two instructions"));
        }

        // Validate that it contains only allowed operations.
        for i in &instructions {
            match *i {
                Instruction::I32Const(_)
                    | Instruction::I64Const(_)
                    | Instruction::F32Const(_)
                    | Instruction::F64Const(_)
                    | Instruction::GetGlobal(_) => (),
                _ => {
                    return Err(Error::Invalid(
                        "initializer expression may only be a constant or get_global"));
                }
            }
        }
        Ok(instructions)
    }
}
