use arbitrary_int::u3;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterAccess {
    LowByte,
    HighByte,
    Word,
    DoubleWord,
    QuadWord,
}

pub type Register = (Registers, RegisterAccess);

#[derive(Debug, Clone, PartialEq)]
pub struct Registers {
    pub name: &'static str,
    pub supports_high_byte: bool,
    pub index: u8,
}

impl Registers {
    pub fn requires_rex_flag(&self) -> bool {
        return self.index & 0b1000 != 0;
    }

    pub fn as_u3(&self) -> u3 {
        // just the 3 lower parts are relevant here
        return u3::from(self.index & 0b0111);
    }

    pub const fn new(name: &'static str, supports_high_byte: bool, index: u8) -> Self {
        Self {
            name,
            supports_high_byte,
            index,
        }
    }

    pub fn get_name(&self, access: RegisterAccess) -> String {
        match access {
            RegisterAccess::LowByte => String::from(self.name) + "L",
            RegisterAccess::HighByte => {
                if !self.supports_high_byte {
                    panic!("{:?} doesn't support accessing the high byte.", self);
                }
                String::from(self.name) + "H"
            }
            RegisterAccess::Word => {
                String::from(self.name)
                    + match self.index {
                        // "A..D"
                        0b0000..=0b0011 => "X",
                        // "SP..DI", no suffix
                        0b0100..=0b0111 => "",
                        // "R8-R15",
                        0b1000..=0b1111 => "W",
                        _ => unreachable!(),
                    }
            }
            RegisterAccess::DoubleWord => {
                String::from(match self.index {
                    // "A..D" & "SP..DI"
                    0b0000..=0b0111 => "E",
                    // "R8-R15",
                    0b1000..=0b1111 => "",
                    _ => unreachable!(),
                }) + self.name
                    + match self.index {
                        // "A..D" & "SP..DI"
                        0b0000..=0b0111 => "",
                        // "R8-R15",
                        0b1000..=0b1111 => "D",
                        _ => unreachable!(),
                    }
            }
            RegisterAccess::QuadWord => {
                String::from(match self.index {
                    // "A..D" & "SP..DI"
                    0b0000..=0b0111 => "R",
                    // "R8-R15",
                    0b1000..=0b1111 => "",
                    _ => unreachable!(),
                }) + self.name
            }
        }
    }
}

pub const ACCUMULATOR: Registers = Registers::new("A", true, 0b0000);
pub const COUNTER: Registers = Registers::new("C", true, 0b0001);
pub const EXTENDED_ACCUMULATOR: Registers = Registers::new("D", true, 0b0010);
pub const BASE_INDEX: Registers = Registers::new("B", true, 0b0011);

pub const STACK_POINTER: Registers = Registers::new("SP", false, 0b0100);
pub const BASE_POINTER: Registers = Registers::new("BP", false, 0b0101);
pub const STREAM_INDEX: Registers = Registers::new("SI", false, 0b0110);
pub const DESTINATION_INDEX: Registers = Registers::new("DI", false, 0b0111);

pub const GPR_8: Registers = Registers::new("R8", false, 0b1000);
const _R9: Registers = Registers::new("R9", false, 0b1001);
const _R10: Registers = Registers::new("R10", false, 0b1010);
const _R11: Registers = Registers::new("R11", false, 0b1011);
const _R12: Registers = Registers::new("R12", false, 0b1100);
pub const GPR_13: Registers = Registers::new("R13", false, 0b1101);
const _R14: Registers = Registers::new("R14", false, 0b1110);
const _R15: Registers = Registers::new("R15", false, 0b1111);

pub const AL: Register = (ACCUMULATOR, RegisterAccess::LowByte);
pub const AH: Register = (ACCUMULATOR, RegisterAccess::HighByte);
pub const AX: Register = (ACCUMULATOR, RegisterAccess::Word);
pub const EAX: Register = (ACCUMULATOR, RegisterAccess::DoubleWord);
pub const RAX: Register = (ACCUMULATOR, RegisterAccess::QuadWord);

pub const CL: Register = (COUNTER, RegisterAccess::LowByte);
pub const CH: Register = (COUNTER, RegisterAccess::HighByte);
pub const CX: Register = (COUNTER, RegisterAccess::Word);
pub const ECX: Register = (COUNTER, RegisterAccess::DoubleWord);
pub const RCX: Register = (COUNTER, RegisterAccess::QuadWord);

pub const DL: Register = (EXTENDED_ACCUMULATOR, RegisterAccess::LowByte);
pub const DH: Register = (EXTENDED_ACCUMULATOR, RegisterAccess::HighByte);
pub const DX: Register = (EXTENDED_ACCUMULATOR, RegisterAccess::Word);
pub const EDX: Register = (EXTENDED_ACCUMULATOR, RegisterAccess::DoubleWord);
pub const RDX: Register = (EXTENDED_ACCUMULATOR, RegisterAccess::QuadWord);

pub const BL: Register = (BASE_INDEX, RegisterAccess::LowByte);
pub const BH: Register = (BASE_INDEX, RegisterAccess::HighByte);
pub const BX: Register = (BASE_INDEX, RegisterAccess::Word);
pub const EBX: Register = (BASE_INDEX, RegisterAccess::DoubleWord);
pub const RBX: Register = (BASE_INDEX, RegisterAccess::QuadWord);

pub const SPL: Register = (STACK_POINTER, RegisterAccess::LowByte);
pub const SP: Register = (STACK_POINTER, RegisterAccess::Word);
pub const ESP: Register = (STACK_POINTER, RegisterAccess::DoubleWord);
pub const RSP: Register = (STACK_POINTER, RegisterAccess::QuadWord);

pub const BPL: Register = (BASE_POINTER, RegisterAccess::LowByte);
pub const BP: Register = (BASE_POINTER, RegisterAccess::Word);
pub const EBP: Register = (BASE_POINTER, RegisterAccess::DoubleWord);
pub const RBP: Register = (BASE_POINTER, RegisterAccess::QuadWord);

pub const R15: Register = (_R15, RegisterAccess::QuadWord);
