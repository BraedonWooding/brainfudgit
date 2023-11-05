use arbitrary_int::u3;

use super::instruction::{EncodedRegister, RexPrefixEncoding, AddressingMode, Displacement};

#[derive(Clone, Debug)]
pub enum Operand {
    Register(Register),
    MemoryBaseRegister(MemoryBaseRegister),
    Immediate(usize),
}

#[derive(Clone, Debug)]
pub enum MemoryBaseRegister {
    /// Only register, no displacement
    Register(Register),
    /// Displacement only
    DisplacementOnly(Register, Displacement),
    /// SIB
    ScaledIndex(ScaledIndex),
}

pub struct ScaledIndex {
    base: Register,
    index: Register,
    displacement: Displacement,
    /// max scale is 8
    scale: u8,
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum ImmediateRegister {
    AL = 0,
    AX = 1,
    RAX = 1,
}

#[derive(Debug, Clone)]
pub enum RegisterType {
    OneByte,
    TwoBytes,
    FourBytes,
    EightBytes,
}

/// What kind of prefix should this register be used with
/// This is built to somewhat easily extend into vectorisation
#[derive(Debug, Clone)]
pub enum PrefixType {
    Rex(RexType),
}

#[derive(Debug, Clone)]
pub enum RexType {
    /// Should be omitted!
    /// for example AH register cannot be written to with a rex prefix
    Omitted,

    /// Can either be omitted or the X/B flag has to be 0
    OmittedOrFlagZero,

    /// The X/B flag has to be 0
    FlagZero,

    /// The X/B flag has to be 1
    FlagOne,
}

/// https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
/// These represent the 8 bit versions of the 16 bit registers AX/BX/CX/DX
/// you can't access these directly 
#[derive(Debug, Clone)]
pub enum Registers {
    AL = (0b000, RegisterType::OneByte, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    BL = (0b001, RegisterType::OneByte, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    CL = (0b010, RegisterType::OneByte, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    DL = (0b011, RegisterType::OneByte, PrefixType::Rex(RexType::OmittedOrFlagZero)),

    // These are used when a rex prefix isn't used
    AH = (0b100, RegisterType::OneByte, PrefixType::Rex(RexType::Omitted)),
    BH = (0b101, RegisterType::OneByte, PrefixType::Rex(RexType::Omitted)),
    CH = (0b110, RegisterType::OneByte, PrefixType::Rex(RexType::Omitted)),
    DH = (0b111, RegisterType::OneByte, PrefixType::Rex(RexType::Omitted)),
    
    // Any rex prefix will cause this to be used (except if Wide is used)
    SPL = (0b100, RegisterType::OneByte, PrefixType::Rex(RexType::FlagZero)),
    BPL = (0b101, RegisterType::OneByte, PrefixType::Rex(RexType::FlagZero)),
    SIL = (0b110, RegisterType::OneByte, PrefixType::Rex(RexType::FlagZero)),
    DIL = (0b111, RegisterType::OneByte, PrefixType::Rex(RexType::FlagZero)),

    // These are the same as above but with REX.X = 1
    R8B = (0b000, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R9B = (0b001, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R10B = (0b010, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R11B = (0b011, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R12B = (0b100, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R13B = (0b101, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R14B = (0b110, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),
    R15B = (0b111, RegisterType::OneByte, PrefixType::Rex(RexType::FlagOne)),

    // == TWO BYTES == //

    AX = (0b000, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    BX = (0b001, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    CX = (0b010, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    DX = (0b011, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    SP = (0b100, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    BP = (0b101, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    SI = (0b110, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    DI = (0b111, RegisterType::TwoBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),

    // These are the same as above but with REX.X = 1
    R8W = (0b000, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R9W = (0b001, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R10W = (0b010, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R11W = (0b011, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R12W = (0b100, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R13W = (0b101, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R14W = (0b110, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    R15W = (0b111, RegisterType::TwoBytes, PrefixType::Rex(RexType::FlagOne)),
    
    // == Four Bytes == //

    EAX = (0b000, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    EBX = (0b001, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    ECX = (0b010, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    EDX = (0b011, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    ESP = (0b100, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    EBP = (0b101, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    ESI = (0b110, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    EDI = (0b111, RegisterType::FourBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),

    // These are the same as above but with REX.X = 1
    R8D = (0b000, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R9D = (0b001, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R10D = (0b010, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R11D = (0b011, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R12D = (0b100, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R13D = (0b101, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R14D = (0b110, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    R15D = (0b111, RegisterType::FourBytes, PrefixType::Rex(RexType::FlagOne)),
    
    // == Eight Bytes == //

    RAX = (0b000, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RBX = (0b001, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RCX = (0b010, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RDX = (0b011, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RSP = (0b100, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RBP = (0b101, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RSI = (0b110, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),
    RDI = (0b111, RegisterType::EightBytes, PrefixType::Rex(RexType::OmittedOrFlagZero)),

    // These are the same as above but with REX.X = 1
    R8 = (0b000, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R9 = (0b001, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R10 = (0b010, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R11 = (0b011, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R12 = (0b100, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R13 = (0b101, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R14 = (0b110, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
    R15 = (0b111, RegisterType::EightBytes, PrefixType::Rex(RexType::FlagOne)),
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Register {
    RAX = 0b0000,
    RCX = 0b0001,
    RDX = 0b0010,
    RBX = 0b0011,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    // The first bit becomes a rex prefix
    R8 = 0b1000,
    R9 = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111,
}

impl Register {
    pub fn encode_register(&self) -> EncodedRegister {
        // https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
        // X.Reg, in this case our encoded register is just encoding the Y part of X.YYY
        // we'll need to set REX flags X & B when using dest & src registers
        return EncodedRegister::new_with_raw_value(u3::from((*self as u8) & 0x7));
    }
}

impl EncodedRegister {
    pub fn from_registers(dst: Operand, src: Operand) -> ((EncodedRegister, EncodedRegister), RexPrefixEncoding) {
        let rex = RexPrefixEncoding::from_operands(dst, src);
        let (Operand::Register(dst, _), Operand::Register(src, _)) = (dst, src);
        ((dst.encode_register(), src.encode_register()), rex)
    }
    
    pub fn from_register(dst: Operand) -> (EncodedRegister, RexPrefixEncoding) {
        let rex = RexPrefixEncoding::from_operand(dst);
        let Operand::Register(dst, _) = dst;
        (dst.encode_register(), rex)
    }
}

impl AddressingMode {
    pub fn from_displacement(displacement: Option<Displacement>) -> AddressingMode {
        match displacement {
            Some(Displacement::ZeroByteDisplacement) => AddressingMode::ZeroByteDisplacement,
            Some(Displacement::OneByteDisplacement(_)) => AddressingMode::OneByteDisplacement,
            Some(Displacement::FourByteDisplacement(_)) => AddressingMode::FourByteDisplacement,
            None => AddressingMode::RegisterDirect,
        }
    }

    pub fn from_operand(op: Operand) -> AddressingMode {
        match op {
            Operand::MemoryBaseRegister(reg) => AddressingMode::from_displacement(Some(reg.displacement)),
            Operand::Register(_) => AddressingMode::from_displacement(displacement),
            Operand::Immediate(_) => unreachable!("This shouldn't be reached can't get addressing mode from an immediate"),
        }
    }
}

impl RexPrefixEncoding {
    pub fn as_u8(&self) -> u8 {
        self.bits() as u8
    }

    pub fn from_operands(dst: Operand, src: Operand) -> RexPrefixEncoding {
        let mut result = RexPrefixEncoding::Base | RexPrefixEncoding::W;
        // TODO: This could be written cleaner

        if let Operand::Register(dst, _) = dst {
            if dst as u8 > 7 {
                result |= RexPrefixEncoding::B;
            }
        }
        if let Operand::Register(src, _) = src {
            if src as u8 > 7 {
                result |= RexPrefixEncoding::R;
            }
        }

        if let Operand::MemoryBaseRegister(reg) = dst {
            if reg.base as u8 > 7 {
                result |= RexPrefixEncoding::B;
            }
            if reg.index as u8 > 7 {
                result |= RexPrefixEncoding::X;
            }
        }

        if let Operand::MemoryBaseRegister(reg) = src {
            if reg.base as u8 > 7 {
                result |= RexPrefixEncoding::B;
            }
            if reg.index as u8 > 7 {
                result |= RexPrefixEncoding::X;
            }
        }

        result
    }

    pub fn from_operand(dst: Operand) -> RexPrefixEncoding {
        // re-using above, so that we don't need to parse this into registers before we use it
        RexPrefixEncoding::from_operands(dst, Operand::Immediate(0))
    }
}