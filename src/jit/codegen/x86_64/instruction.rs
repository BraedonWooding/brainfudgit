use std::io::{self, BufWriter, ErrorKind, Write};

use arbitrary_int::u3;
use bitbybit::{bitenum, bitfield};
use bitflags::bitflags;

use num_traits::ops::bytes::ToBytes;

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
    pub mod_rm: Option<ModRM>,

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Displacement {
    ZeroByteDisplacement,
    OneByteDisplacement(u8),
    FourByteDisplacement(u32),
}

impl Displacement {
    pub fn coerce_to_fourbytes(&self) -> Displacement {
        match *self {
            Displacement::ZeroByteDisplacement => Displacement::FourByteDisplacement(0),
            Displacement::OneByteDisplacement(i) => {
                Displacement::FourByteDisplacement(u32::from(i))
            }
            Displacement::FourByteDisplacement(i) => Displacement::FourByteDisplacement(i),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Immediate {
    Imm8(u8),
    Imm32(u32),
    /// possible but not with a displacement
    Imm64(u64),
}

impl Immediate {
    pub fn is_zero(&self) -> bool {
        return match *self {
            Immediate::Imm8(i) => i == 0,
            Immediate::Imm32(i) => i == 0,
            Immediate::Imm64(i) => i == 0,
        };
    }
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
    #[derive(Clone, Copy, Debug)]
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

impl RexPrefixEncoding {
    pub fn as_u8(&self) -> u8 {
        self.bits() as u8
    }
}

/// Special mode that is enabled if the register is 101 (EBP) & MOD (addressing mode) = 00
/// is a full byte.
///
/// Defined as displacement (as usual) + base + index * scale.
#[bitfield(u8, default: 0)]
#[derive(Debug)]
#[repr(C)]
pub struct ScaledIndexByte {
    #[bits(5..=7, rw)]
    pub scale: u3,

    #[bits(3..=5, rw)]
    pub index: u3,

    #[bits(0..=2, rw)]
    pub base: u3,
}

/// This primarily specifies addressing mode, a source/destination register, and optionally an opcode extension
/// Default is 0b11 since it's the RegisterDirect.
#[bitfield(u8, default: 0b11_000_000)]
#[derive(Debug)]
#[repr(C)]
pub struct ModRM {
    #[bits(6..=7, rw)]
    pub addressing_mode: AddressingMode,

    /// REX.B extends this to access upper registers
    #[bits(0..=2, rw)]
    pub register_memory: u3,

    #[bits(3..=5, rw)]
    pub opcode_extension: u3,

    /// REX.R extends this to access upper registers
    #[bits(3..=5, rw)]
    pub register: u3,
}

impl ModRM {
    pub fn new_if_opcode(opt_opcode_extension: Option<u3>) -> Option<ModRM> {
        if let Some(opcode_extension) = opt_opcode_extension {
            Some(ModRM::default().with_opcode_extension(opcode_extension))
        } else {
            None
        }
    }
}

impl Instruction {
    fn emit_opt_byte<const COUNT: usize, T: ToBytes<Bytes = [u8; COUNT]>>(
        writer: &mut BufWriter<&mut [u8]>,
        value: Option<T>,
    ) -> io::Result<usize> {
        if let Some(value) = value {
            writer.write(&ToBytes::to_le_bytes(&value))
        } else {
            Ok(0)
        }
    }

    pub fn len(&self) -> usize {
        let mut buf = [0; 15];
        self.read(&mut buf)
            .expect("We pass a valid buffer we should expect a valid result here.")
    }

    pub fn read(&self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.len() < 15 {
            return Err(io::Error::new(
                ErrorKind::Interrupted,
                "Requires at-least 15 bytes of buffer",
            ));
        } else {
            let mut writer = BufWriter::new(buf);
            let mut count = 0;

            // emit prefixes
            count += Instruction::emit_opt_byte(&mut writer, self.prefix.mandatory_prefix)?;
            count += Instruction::emit_opt_byte(
                &mut writer,
                self.prefix
                    .two_byte_opcode
                    .map(|opcode_flag| opcode_flag as u8),
            )?;
            count +=
                Instruction::emit_opt_byte(&mut writer, self.prefix.rex.map(|rex| rex.as_u8()))?;

            // emit opcode/s
            count += Instruction::emit_opt_byte(&mut writer, Some(self.primary_opcode))?;
            count += Instruction::emit_opt_byte(&mut writer, self.secondary_opcode)?;

            // modrm & sib
            count +=
                Instruction::emit_opt_byte(&mut writer, self.mod_rm.map(|modrm| modrm.raw_value))?;
            count += Instruction::emit_opt_byte(&mut writer, self.sib.map(|sib| sib.raw_value))?;

            // displacement & immediate
            if let Some(displacement) = self.displacement {
                count += match displacement {
                    Displacement::ZeroByteDisplacement => 0,
                    Displacement::OneByteDisplacement(byte) => {
                        Instruction::emit_opt_byte(&mut writer, Some(byte))?
                    }
                    Displacement::FourByteDisplacement(double_word) => {
                        Instruction::emit_opt_byte(&mut writer, Some(double_word))?
                    }
                }
            }
            if let Some(immediate) = self.immediate {
                count += match immediate {
                    Immediate::Imm8(byte) => Instruction::emit_opt_byte(&mut writer, Some(byte))?,
                    Immediate::Imm32(double_word) => {
                        Instruction::emit_opt_byte(&mut writer, Some(double_word))?
                    }
                    Immediate::Imm64(quad_word) => {
                        Instruction::emit_opt_byte(&mut writer, Some(quad_word))?
                    }
                }
            }

            Ok(count)
        }
    }
}
