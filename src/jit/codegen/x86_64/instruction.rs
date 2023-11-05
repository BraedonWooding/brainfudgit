use arbitrary_int::u3;
use bitbybit::{bitfield, bitenum};
use bitflags::bitflags;

use super::operand::Operand;

/// This is the output of any opcode and pretty much can be outputted directly to instruction stream
/// only difference is that we don't encode Option<>
/// 
/// Structure is: (everything is 1 byte unless otherwise specified)
/// Prefix { MandatoryPrefix | TwoByteOpcodeFlag | REX } | PrimaryOpcode | SecondaryOpcode | MOD/RM | SIB | Displacement (1/4) | Immediate (1/4/8)
/// this would give a max length of Prefix (3), Primary & Secondary (2), ModRM + SIB + Displacement (6) + Immediate (4, can't have 8 & displacement)
/// which would be 15!  Which matches the max length of an instruction that can be decoded (so napkin math-wise this is good)
#[derive(Clone, Debug)]
pub struct Instruction {
    pub prefix: Prefix,

    pub primary_opcode: u8,
    pub secondary_opcode: Option<u8>,
    pub mod_rm: ModRM,

    pub sib: Option<ScaledIndexByte>,
    pub displacement: Option<Displacement>,
    pub immediate: Option<Immediate>,
}


#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum TwoByteOpcode {
    Value = 0x0F,
}

/// The encodings that can be emitted prior to the instruction
/// written this way since there is an order to the prefixes
#[derive(Clone, Debug)]
pub struct Prefix {
    pub mandatory_prefix: Option<u8>,
    /// If set it should just be set to `TwoByteOpcode.Value`
    pub two_byte_opcode: Option<TwoByteOpcode>,
    pub rex: Option<RexPrefixEncoding>,
}

#[derive(Clone, Debug)]
pub enum Displacement {
    ZeroByteDisplacement,
    OneByteDisplacement(u8),
    FourByteDisplacement(u32),
}

#[derive(Clone, Debug)]
pub enum Immediate {
    Imm8(u8),
    Imm32(u32),
    /// possible but not with a displacement
    Imm64(u64),
}

/// The addressing mode of the registers, this is the first 2 bits of the ModRM
#[bitenum(u2, exhaustive: true)]
pub enum AddressingMode {
    /// Dereference the memory location at the register but there is no additional displacement
    ZeroByteDisplacement = 0b00,
    /// Dereference the memory location at the register and apply the 2-byte displacement that is stored 1 byte after MOD R/M but before the constant/immediate
    OneByteDisplacement = 0b01,
    /// Dereference the memory location at the register and apply the 4-byte displacement that is stored 1 byte after MOD R/M but before the constant/immediate
    FourByteDisplacement = 0b10,
    /// No indirect lookups of memory locations just use the direct value stored in the register
    RegisterDirect = 0b11,
}

bitflags! {
    pub struct RexPrefixEncoding : u8 {
        const Base  = 0b0100_000;
        /// Wide instruction (64 bit instead of 32)
        const W     = 0b0001_000;
        /// Extends the `register` field in MOD/RM
        const R     = 0b0000_100;
        /// Extends the `index` field in SIB
        const X     = 0b0000_010;
        /// Extends the `register_or_memory` field in MOD/RM or the `base` field in SIB
        const B     = 0b0000_001;
    }
}

#[derive(Debug)]
#[bitenum(u3, exhaustive: true)]
pub enum EncodedRegister {
    AX = 0b000,
    CX = 0b001,
    DX = 0b010,
    BX = 0b011,
    /// Illegal argument for SIB byte but otherwise valid
    /// when used in MOD/RM specifies there will be an SIB byte
    SP = 0b100,
    /// Used to indicate displacement only mode also 
    BP = 0b101,
    SI = 0b110,
    DI = 0b111,
}

/// Special mode that is enabled if the register is 101 (EBP) & MOD (addressing mode) = 00
/// is a full byte.
/// 
/// Defined as displacement (as usual) + base + index * scale.
#[bitfield(u8)]
pub struct ScaledIndexByte {
    #[bits(5..=7, r)]
    pub scale: u3,

    #[bits(3..=5, r)]
    pub index: EncodedRegister,

    #[bits(0..=2, r)]
    pub base: EncodedRegister,
}

/// This primarily specifies addressing mode, a source/destination register, and optionally an opcode extension
#[bitfield(u8)]
pub struct ModRM {
    #[bits(6..=7, r)]
    pub addressing_mode: AddressingMode,

    #[bits(3..=5, r)]
    pub register: ModRMRegister,

    /// REX.B extends this to access upper registers
    #[bits(0..=2, r)]
    pub register_memory: EncodedRegister,
}

/// This is like an "enum" but the fields are overlapped
/// which one is set will be based upon the opcode
#[bitfield(u3)]
pub struct ModRMRegister {
    #[bits(0..=2, r)]
    pub opcode_extension: u3,

    /// REX.R extends this to access upper registers
    #[bits(0..=2, r)]
    pub register: EncodedRegister,
}