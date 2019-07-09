use crate::Error;
use crate::module::{self, BlockType};
use crate::util::{read_varu32, read_varu1, read_vari32, read_vari64, read_f32, read_f64};
use num_traits::FromPrimitive;
use std::io;

#[derive(Primitive, Debug)]
pub enum Opcode {
    // Control flow operators
    Unreachable = 0x00,
    Nop = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0b,
    Br = 0x0c,
    BrIf = 0x0d,
    BrTable = 0x0e,
    Return = 0x0f,

    // Call operators
    Call = 0x10,
    CallIndirect = 0x11,

    // Parametric operators
    Drop = 0x1a,
    Select = 0x1b,

    // Variable access
    GetLocal = 0x20,
    SetLocal = 0x21,
    TeeLocal = 0x22,
    GetGlobal = 0x23,
    SetGlobal = 0x24,

    // Memory-related operators
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2a,
    F64Load = 0x2b,
    I32Load8S = 0x2c,
    I32Load8U = 0x2d,
    I32Load16S = 0x2e,
    I32Load16U = 0x2f,
    I64Load8S = 0x30,
    I64Load8U = 0x31,
    I64Load16S = 0x32,
    I64Load16U = 0x33,
    I64Load32S = 0x34,
    I64Load32U = 0x35,

    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3a,
    I32Store16 = 0x3b,
    I64Store8 = 0x3c,
    I64Store16 = 0x3d,
    I64Store32 = 0x3e,

    CurrentMemory = 0x3f,
    GrowMemory = 0x40,

    // Constants
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,

    // Comparison operators
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I32LeS = 0x4c,
    I32LeU = 0x4d,
    I32GeS = 0x4e,
    I32GeU = 0x4f,

    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5a,

    F32Eq = 0x5b,
    F32Ne = 0x5c,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F32Le = 0x5f,
    F32Ge = 0x60,

    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,

    // Numeric operators
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32PopCnt = 0x69,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I32DivU = 0x6e,
    I32RemS = 0x6f,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I32Rotl = 0x77,
    I32Rotr = 0x78,

    I64Clz = 0x79,
    I64Ctz = 0x7a,
    I64PopCnt = 0x7b,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,
    I64Shl = 0x86,
    I64ShrS = 0x87,
    I64ShrU = 0x88,
    I64Rotl = 0x89,
    I64Rotr = 0x8a,

    F32Abs = 0x8b,
    F32Neg = 0x8c,
    F32Ceil = 0x8d,
    F32Floor = 0x8e,
    F32Trunc = 0x8f,
    F32Nearest = 0x90,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F32Min = 0x96,
    F32Max = 0x97,
    F32Copysign = 0x98,

    F64Abs = 0x99,
    F64Neg = 0x9a,
    F64Ceil = 0x9b,
    F64Floor = 0x9c,
    F64Trunc = 0x9d,
    F64Nearest = 0x9e,
    F64Sqrt = 0x9f,
    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,
    F64Min = 0xa4,
    F64Max = 0xa5,
    F64Copysign = 0xa6,

    // Conversions
    I32WrapI64 = 0xa7,
    I32TruncSF32 = 0xa8,
    I32TruncUF32 = 0xa9,
    I32TruncSF64 = 0xaa,
    I32TruncUF64 = 0xab,
    I64ExtendSI32 = 0xac,
    I64ExtendUI32 = 0xad,
    I64TruncSF32 = 0xae,
    I64TruncUF32 = 0xaf,
    I64TruncSF64 = 0xb0,
    I64TruncUF64 = 0xb1,
    F32ConvertSI32 = 0xb2,
    I32ConvertUI32 = 0xb3,
    F32ConvertSI64 = 0xb4,
    F32ConvertUI64 = 0xb5,
    F32DemoteF64 = 0xb6,
    F64ConvertSI32 = 0xb7,
    F64ConvertUI32 = 0xb8,
    F64ConvertSI64 = 0xb9,
    F64ConvertUI64 = 0xba,
    F64PromoteF32 = 0xbb,

    // Reinterpretations
    I32ReinterpretF32 = 0xbc,
    I64ReinterpretF64 = 0xbd,
    F32ReinterpretI32 = 0xbe,
    F64ReinterpretI64 = 0xbf,
}

impl Opcode {
    pub fn from_u8(byte: u8) -> Result<Self, Error> {
        <Self as FromPrimitive>::from_u8(byte)
            .ok_or(Error::Invalid("invalid opcode"))
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Unreachable,
    Nop,
    Block(BlockType),
    Loop(BlockType),
    If(BlockType),
    Else,
    End,
    Br(u32),
    BrIf(u32),
    BrTable {
        target_table: Vec<u32>,
        default_target: u32,
    },
    Call(u32),
    CallIndirect(u32), // also a 2nd reserved value
    Return,
    Drop,
    Select,

    GetLocal(usize),
    SetLocal(usize),
    TeeLocal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    I32Load(MemoryImmediate),
    I64Load(MemoryImmediate),
    F32Load(MemoryImmediate),
    F64Load(MemoryImmediate),
    I32Load8S(MemoryImmediate),
    I32Load8U(MemoryImmediate),
    I32Load16S(MemoryImmediate),
    I32Load16U(MemoryImmediate),
    I64Load8S(MemoryImmediate),
    I64Load8U(MemoryImmediate),
    I64Load16S(MemoryImmediate),
    I64Load16U(MemoryImmediate),
    I64Load32S(MemoryImmediate),
    I64Load32U(MemoryImmediate),
    I32Store(MemoryImmediate),
    I64Store(MemoryImmediate),
    F32Store(MemoryImmediate),
    F64Store(MemoryImmediate),
    I32Store8(MemoryImmediate),
    I32Store16(MemoryImmediate),
    I64Store8(MemoryImmediate),
    I64Store16(MemoryImmediate),
    I64Store32(MemoryImmediate),
    CurrentMemory,  // actually takes a varuint1
    GrowMemory,     // actually takes a varuint1

    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

    // Comparison operators
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    // Numeric operators
    I32Clz,
    I32Ctz,
    I32PopCnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    I64Clz,
    I64Ctz,
    I64PopCnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    // Conversions
    I32WrapI64,
    I32TruncSF32,
    I32TruncUF32,
    I32TruncSF64,
    I32TruncUF64,
    I64ExtendSI32,
    I64ExtendUI32,
    I64TruncSF32,
    I64TruncUF32,
    I64TruncSF64,
    I64TruncUF64,
    F32ConvertSI32,
    I32ConvertUI32,
    F32ConvertSI64,
    F32ConvertUI64,
    F32DemoteF64,
    F64ConvertSI32,
    F64ConvertUI32,
    F64ConvertSI64,
    F64ConvertUI64,
    F64PromoteF32,

    // Reinterpretations
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
}

impl module::Read for Instruction {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = [0u8];
        r.read_exact(&mut buf).map_err(Error::IO)?;
        let op = Opcode::from_u8(buf[0])?;
        Ok(match op {
            Opcode::Unreachable => Instruction::Unreachable,
            Opcode::Nop => Instruction::Nop,
            Opcode::Block => Instruction::Block(BlockType::read(r)?),
            Opcode::Loop => Instruction::Loop(BlockType::read(r)?),
            Opcode::If => Instruction::If(BlockType::read(r)?),
            Opcode::Else => Instruction::Else,
            Opcode::End => Instruction::End,
            Opcode::Br => Instruction::Br(read_varu32(r)?),
            Opcode::BrIf => Instruction::BrIf(read_varu32(r)?),
            Opcode::BrTable => {
                let count = read_varu32(&mut r)?;
                let mut target_table = Vec::with_capacity(count as usize);
                for _ in 0 .. count {
                    target_table.push(read_varu32(&mut r)?);
                }
                let default_target = read_varu32(&mut r)?;
                Instruction::BrTable {
                    target_table,
                    default_target,
                }
            }
            Opcode::Return => Instruction::Return,

            Opcode::Call => Instruction::Call(read_varu32(r)?),
            Opcode::CallIndirect => {
                let type_index = read_varu32(&mut r)?;
                if read_varu1(&mut r)? {
                    return Err(Error::Invalid("CallIndirect 2nd immediate must be 0"));
                }
                Instruction::CallIndirect(type_index)
            }
            Opcode::Drop => Instruction::Drop,
            Opcode::Select => Instruction::Select,

            Opcode::GetLocal => Instruction::GetLocal(read_varu32(r)? as usize),
            Opcode::SetLocal => Instruction::SetLocal(read_varu32(r)? as usize),
            Opcode::TeeLocal => Instruction::TeeLocal(read_varu32(r)? as usize),
            Opcode::GetGlobal => Instruction::GetGlobal(read_varu32(r)? as usize),
            Opcode::SetGlobal => Instruction::SetGlobal(read_varu32(r)? as usize),

            Opcode::I32Load => Instruction::I32Load(MemoryImmediate::read(r)?),
            Opcode::I64Load => Instruction::I64Load(MemoryImmediate::read(r)?),
            Opcode::F32Load => Instruction::F32Load(MemoryImmediate::read(r)?),
            Opcode::F64Load => Instruction::F64Load(MemoryImmediate::read(r)?),
            Opcode::I32Load8S => Instruction::I32Load8S(MemoryImmediate::read(r)?),
            Opcode::I32Load8U => Instruction::I32Load8U(MemoryImmediate::read(r)?),
            Opcode::I32Load16S => Instruction::I32Load16S(MemoryImmediate::read(r)?),
            Opcode::I32Load16U => Instruction::I32Load16U(MemoryImmediate::read(r)?),
            Opcode::I64Load8S => Instruction::I64Load8S(MemoryImmediate::read(r)?),
            Opcode::I64Load8U => Instruction::I64Load8U(MemoryImmediate::read(r)?),
            Opcode::I64Load16S => Instruction::I64Load16S(MemoryImmediate::read(r)?),
            Opcode::I64Load16U => Instruction::I64Load16U(MemoryImmediate::read(r)?),
            Opcode::I64Load32S => Instruction::I64Load32S(MemoryImmediate::read(r)?),
            Opcode::I64Load32U => Instruction::I64Load32U(MemoryImmediate::read(r)?),

            Opcode::I32Store => Instruction::I32Store(MemoryImmediate::read(r)?),
            Opcode::I64Store => Instruction::I64Store(MemoryImmediate::read(r)?),
            Opcode::F32Store => Instruction::F32Store(MemoryImmediate::read(r)?),
            Opcode::F64Store => Instruction::F64Store(MemoryImmediate::read(r)?),
            Opcode::I32Store8 => Instruction::I32Store8(MemoryImmediate::read(r)?),
            Opcode::I32Store16 => Instruction::I32Store16(MemoryImmediate::read(r)?),
            Opcode::I64Store8 => Instruction::I64Store8(MemoryImmediate::read(r)?),
            Opcode::I64Store16 => Instruction::I64Store16(MemoryImmediate::read(r)?),
            Opcode::I64Store32 => Instruction::I64Store32(MemoryImmediate::read(r)?),

            Opcode::I32Const => Instruction::I32Const(read_vari32(r)?),
            Opcode::I64Const => Instruction::I64Const(read_vari64(r)?),
            Opcode::F32Const => Instruction::F32Const(read_f32(r)?),
            Opcode::F64Const => Instruction::F64Const(read_f64(r)?),

            Opcode::CurrentMemory => {
                if read_varu1(r)? {
                    return Err(Error::Invalid("CurrentMemory immediate must be 0"));
                }
                Instruction::CurrentMemory
            }
            Opcode::GrowMemory => {
                if read_varu1(r)? {
                    return Err(Error::Invalid("GrowMemory immediate must be 0"));
                }
                Instruction::GrowMemory
            }

            // Comparison operators
            Opcode::I32Eqz => Instruction::I32Eqz,
            Opcode::I32Eq => Instruction::I32Eq,
            Opcode::I32Ne => Instruction::I32Ne,
            Opcode::I32LtS => Instruction::I32LtS,
            Opcode::I32LtU => Instruction::I32LtU,
            Opcode::I32GtS => Instruction::I32GtS,
            Opcode::I32GtU => Instruction::I32GtU,
            Opcode::I32LeS => Instruction::I32LeS,
            Opcode::I32LeU => Instruction::I32LeU,
            Opcode::I32GeS => Instruction::I32GeS,
            Opcode::I32GeU => Instruction::I32GeU,

            Opcode::I64Eqz => Instruction::I64Eqz,
            Opcode::I64Eq => Instruction::I64Eq,
            Opcode::I64Ne => Instruction::I64Ne,
            Opcode::I64LtS => Instruction::I64LtS,
            Opcode::I64LtU => Instruction::I64LtU,
            Opcode::I64GtS => Instruction::I64GtS,
            Opcode::I64GtU => Instruction::I64GtU,
            Opcode::I64LeS => Instruction::I64LeS,
            Opcode::I64LeU => Instruction::I64LeU,
            Opcode::I64GeS => Instruction::I64GeS,
            Opcode::I64GeU => Instruction::I64GeU,

            Opcode::F32Eq => Instruction::F32Eq,
            Opcode::F32Ne => Instruction::F32Ne,
            Opcode::F32Lt => Instruction::F32Lt,
            Opcode::F32Gt => Instruction::F32Gt,
            Opcode::F32Le => Instruction::F32Le,
            Opcode::F32Ge => Instruction::F32Ge,

            Opcode::F64Eq => Instruction::F64Eq,
            Opcode::F64Ne => Instruction::F64Ne,
            Opcode::F64Lt => Instruction::F64Lt,
            Opcode::F64Gt => Instruction::F64Gt,
            Opcode::F64Le => Instruction::F64Le,
            Opcode::F64Ge => Instruction::F64Ge,

            // Numeric operators
            Opcode::I32Clz => Instruction::I32Clz,
            Opcode::I32Ctz => Instruction::I32Ctz,
            Opcode::I32PopCnt => Instruction::I32PopCnt,
            Opcode::I32Add => Instruction::I32Add,
            Opcode::I32Sub => Instruction::I32Sub,
            Opcode::I32Mul => Instruction::I32Mul,
            Opcode::I32DivS => Instruction::I32DivS,
            Opcode::I32DivU => Instruction::I32DivU,
            Opcode::I32RemS => Instruction::I32RemS,
            Opcode::I32RemU => Instruction::I32RemU,
            Opcode::I32And => Instruction::I32And,
            Opcode::I32Or => Instruction::I32Or,
            Opcode::I32Xor => Instruction::I32Xor,
            Opcode::I32Shl => Instruction::I32Shl,
            Opcode::I32ShrS => Instruction::I32ShrS,
            Opcode::I32ShrU => Instruction::I32ShrU,
            Opcode::I32Rotl => Instruction::I32Rotl,
            Opcode::I32Rotr => Instruction::I32Rotr,

            Opcode::I64Clz => Instruction::I64Clz,
            Opcode::I64Ctz => Instruction::I64Ctz,
            Opcode::I64PopCnt => Instruction::I64PopCnt,
            Opcode::I64Add => Instruction::I64Add,
            Opcode::I64Sub => Instruction::I64Sub,
            Opcode::I64Mul => Instruction::I64Mul,
            Opcode::I64DivS => Instruction::I64DivS,
            Opcode::I64DivU => Instruction::I64DivU,
            Opcode::I64RemS => Instruction::I64RemS,
            Opcode::I64RemU => Instruction::I64RemU,
            Opcode::I64And => Instruction::I64And,
            Opcode::I64Or => Instruction::I64Or,
            Opcode::I64Xor => Instruction::I64Xor,
            Opcode::I64Shl => Instruction::I64Shl,
            Opcode::I64ShrS => Instruction::I64ShrS,
            Opcode::I64ShrU => Instruction::I64ShrU,
            Opcode::I64Rotl => Instruction::I64Rotl,
            Opcode::I64Rotr => Instruction::I64Rotr,

            Opcode::F32Abs => Instruction::F32Abs,
            Opcode::F32Neg => Instruction::F32Neg,
            Opcode::F32Ceil => Instruction::F32Ceil,
            Opcode::F32Floor => Instruction::F32Floor,
            Opcode::F32Trunc => Instruction::F32Trunc,
            Opcode::F32Nearest => Instruction::F32Nearest,
            Opcode::F32Sqrt => Instruction::F32Sqrt,
            Opcode::F32Add => Instruction::F32Add,
            Opcode::F32Sub => Instruction::F32Sub,
            Opcode::F32Mul => Instruction::F32Mul,
            Opcode::F32Div => Instruction::F32Div,
            Opcode::F32Min => Instruction::F32Min,
            Opcode::F32Max => Instruction::F32Max,
            Opcode::F32Copysign => Instruction::F32Copysign,

            Opcode::F64Abs => Instruction::F64Abs,
            Opcode::F64Neg => Instruction::F64Neg,
            Opcode::F64Ceil => Instruction::F64Ceil,
            Opcode::F64Floor => Instruction::F64Floor,
            Opcode::F64Trunc => Instruction::F64Trunc,
            Opcode::F64Nearest => Instruction::F64Nearest,
            Opcode::F64Sqrt => Instruction::F64Sqrt,
            Opcode::F64Add => Instruction::F64Add,
            Opcode::F64Sub => Instruction::F64Sub,
            Opcode::F64Mul => Instruction::F64Mul,
            Opcode::F64Div => Instruction::F64Div,
            Opcode::F64Min => Instruction::F64Min,
            Opcode::F64Max => Instruction::F64Max,
            Opcode::F64Copysign => Instruction::F64Copysign,

            // Conversions
            Opcode::I32WrapI64 => Instruction::I32WrapI64,
            Opcode::I32TruncSF32 => Instruction::I32TruncSF32,
            Opcode::I32TruncUF32 => Instruction::I32TruncUF32,
            Opcode::I32TruncSF64 => Instruction::I32TruncSF64,
            Opcode::I32TruncUF64 => Instruction::I32TruncUF64,
            Opcode::I64ExtendSI32 => Instruction::I64ExtendSI32,
            Opcode::I64ExtendUI32 => Instruction::I64ExtendUI32,
            Opcode::I64TruncSF32 => Instruction::I64TruncSF32,
            Opcode::I64TruncUF32 => Instruction::I64TruncUF32,
            Opcode::I64TruncSF64 => Instruction::I64TruncSF64,
            Opcode::I64TruncUF64 => Instruction::I64TruncUF64,
            Opcode::F32ConvertSI32 => Instruction::F32ConvertSI32,
            Opcode::I32ConvertUI32 => Instruction::I32ConvertUI32,
            Opcode::F32ConvertSI64 => Instruction::F32ConvertSI64,
            Opcode::F32ConvertUI64 => Instruction::F32ConvertUI64,
            Opcode::F32DemoteF64 => Instruction::F32DemoteF64,
            Opcode::F64ConvertSI32 => Instruction::F64ConvertSI32,
            Opcode::F64ConvertUI32 => Instruction::F64ConvertUI32,
            Opcode::F64ConvertSI64 => Instruction::F64ConvertSI64,
            Opcode::F64ConvertUI64 => Instruction::F64ConvertUI64,
            Opcode::F64PromoteF32 => Instruction::F64PromoteF32,

            // Reinterpretations
            Opcode::I32ReinterpretF32 => Instruction::I32ReinterpretF32,
            Opcode::I64ReinterpretF64 => Instruction::I64ReinterpretF64,
            Opcode::F32ReinterpretI32 => Instruction::F32ReinterpretI32,
            Opcode::F64ReinterpretI64 => Instruction::F64ReinterpretI64,
        })
    }
}

pub fn read_instructions(bytes: &[u8]) -> Result<Vec<Instruction>, Error> {
    let mut seq = vec![];
    let mut reader = io::Cursor::new(bytes);
    while reader.position() != bytes.len() as u64 {
        let instr = module::Read::read(&mut reader)?;
        seq.push(instr);
    }
    Ok(seq)
}

#[derive(Debug, PartialEq)]
pub struct MemoryImmediate {
    pub alignment: u32,
    pub offset: u32,
}

impl module::Read for MemoryImmediate {
    fn read<R: io::Read>(mut r: R) -> Result<Self, Error> {
        let flags = read_varu32(&mut r)?;
        let offset = read_varu32(&mut r)?;
        let alignment = 1 << flags;
        Ok(MemoryImmediate {
            alignment,
            offset,
        })
    }
}
