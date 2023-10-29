use super::CodeGen;

/*
    - RSP/ESP (Stack Pointer): Data Pointer
    - RBP/EBP (Base Pointer): 

    http://ref.x86asm.net/coder64.html most instructions come from this
*/

pub struct X86_64Codegen {
    bytes: Vec<u8>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Register {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Mnemonic {
    /// We are using the r16/r32/r64 variant so we can use larger registers
    AddRegisters = 0x01,

    /// Perform a math opcode on an i32 to a r16/32/r64 register
    MathImm32 = 0x81,

    /// Perform a math opcode on an i8 to a r16/32/r64 register
    MathImm8 = 0x83,

    /// We are using the r16/r32/r64 variant so we can use larger registers
    SubRegisters = 0x29,
}

impl Mnemonic {
    pub fn mnemonic_for_opcode(opcode: OpCode) -> Mnemonic {
        match opcode {
            OpCode::Add => Mnemonic::AddRegisters,
            OpCode::Sub => Mnemonic::SubRegisters,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Add = 0,
    Sub = 5,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum AddressingMode {
    RegisterDirect = 0xC0,
}

use bitflags::bitflags;

bitflags! {
    pub struct RexPrefixEncoding : u8 {
        const Base  = 0b0100_000;
        const W     = 0b0001_000;
        const R     = 0b0000_100;
        const X     = 0b0000_010;
        const B     = 0b0000_001;
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Operand {
    Register(Register),
    Immediate(usize),
    Memory(Register, usize),
}

impl Register {
    pub fn encode_register(&self) -> u8 {
        // https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
        // X.Reg, in this case our encoded register is just encoding the Y part of X.YYY
        // we'll need to set REX flags X & B when using dest & src registers
        return (*self as u8) & 0x7;
    }
}

impl RexPrefixEncoding {
    pub fn as_u8(&self) -> u8 {
        self.bits() as u8
    }

    pub fn from_registers(dst: Register, src: Register) -> RexPrefixEncoding {
        let mut result = RexPrefixEncoding::Base | RexPrefixEncoding::W;
        if dst as u8 > 7 {
            result |= RexPrefixEncoding::B;
        }
        if src as u8 > 7 {
            result |= RexPrefixEncoding::R;
        }

        result
    }

    pub fn from_register(dst: Register) -> RexPrefixEncoding {
        let mut result = RexPrefixEncoding::Base | RexPrefixEncoding::W;
        if dst as u8 > 7 {
            result |= RexPrefixEncoding::B;
        }

        result
    }
}

impl X86_64Codegen {
    fn emit8(&mut self, byte: u8) {
        self.bytes.push(byte);
    }

    fn emit32(&mut self, value: u32) {
        let bytes = value.to_le_bytes();
        for i in 0..4 {
            self.bytes.push(bytes[i]);
        }
    }

    fn value_fits_in_i8(value: usize) -> bool {
        value <= (i8::MAX as usize) || (((!value) & i8::MIN as usize) == 0)
    }

    fn value_fits_in_i32(value: usize) -> bool {
        value <= (i32::MAX as usize) || (((!value) & i32::MIN as usize) == 0)
    }

    fn encode_math_instruction(&mut self, opcode: OpCode, dst: Operand, src: Operand) {
        match (dst, src) {
            // dst = Math(dst, src)
            (Operand::Register(dst), Operand::Register(src)) => {
                self.emit8(RexPrefixEncoding::from_registers(dst, src).as_u8());
                self.emit8(Mnemonic::mnemonic_for_opcode(opcode) as u8);
                self.emit8(AddressingMode::RegisterDirect as u8 | (src.encode_register() << 3) | dst.encode_register());
            },
            // dst = Math(dst, imm) (8bit/32bit)
            (Operand::Register(dst), Operand::Immediate(imm)) => {
                self.emit8(RexPrefixEncoding::from_register(dst).as_u8());

                if X86_64Codegen::value_fits_in_i8(imm) {
                    self.emit8(Mnemonic::MathImm8 as u8);
                    self.emit8(AddressingMode::RegisterDirect as u8 | opcode as u8 | dst.encode_register());
                    self.emit8(imm as u8);
                } else if X86_64Codegen::value_fits_in_i32(imm) {
                    self.emit8(Mnemonic::MathImm32 as u8);
                    self.emit8(AddressingMode::RegisterDirect as u8 | opcode as u8 | dst.encode_register());
                    self.emit32(imm as u32);
                } else {
                    unreachable!("Only supports i8 & i32");
                }
            },
            _ => unreachable!("Invalid arms dst {:?}, src {:?}", dst, src),
        }
    }

    fn add(&mut self, dst: Operand, src: Operand) {
        self.encode_math_instruction(OpCode::Add, dst, src);
    }

    fn sub(&mut self, dst: Operand, src: Operand) {
        self.encode_math_instruction(OpCode::Sub, dst, src);
    }

    fn windows_call(&mut self) {
        // For a windows call we need to make 32 bytes worth of stack space and 8 bytes to align to 16 byte boundary
        self.sub(Operand::Register(Register::RSP), Operand::Immediate(32 + 8));
        // self.mov(...)
        // self.call(...)

        // Clean up the stack from what we pushed above
        self.add(Operand::Register(Register::RSP), Operand::Immediate(32 + 8));
    }
}

impl CodeGen for X86_64Codegen {
    fn compile(executable: Vec<u8>, bytecode: Vec<crate::bytecode::ByteCode>) {
    }
}