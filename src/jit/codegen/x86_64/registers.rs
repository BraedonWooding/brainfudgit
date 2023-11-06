use arbitrary_int::u3;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterAccess {
    LowByte,
    HighByte,
    LowTwoBytes,
    LowFourBytes,
    LowEightBytes,
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
            RegisterAccess::LowTwoBytes => {
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
            RegisterAccess::LowFourBytes => {
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
            RegisterAccess::LowEightBytes => {
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

pub const A: Registers = Registers::new("A", true, 0b0000);
pub const B: Registers = Registers::new("B", true, 0b0001);
pub const C: Registers = Registers::new("C", true, 0b0010);
pub const D: Registers = Registers::new("D", true, 0b0011);

pub const SP: Registers = Registers::new("SP", false, 0b0100);
pub const BP: Registers = Registers::new("BP", false, 0b0101);
pub const SI: Registers = Registers::new("SI", false, 0b0110);
pub const DI: Registers = Registers::new("DI", false, 0b0111);

pub const R8: Registers = Registers::new("R8", false, 0b1000);
pub const R9: Registers = Registers::new("R9", false, 0b1001);
pub const R10: Registers = Registers::new("R10", false, 0b1010);
pub const R11: Registers = Registers::new("R11", false, 0b1011);
pub const R12: Registers = Registers::new("R12", false, 0b1100);
pub const R13: Registers = Registers::new("R13", false, 0b1101);
pub const R14: Registers = Registers::new("R14", false, 0b1110);
pub const R15: Registers = Registers::new("R15", false, 0b1111);
