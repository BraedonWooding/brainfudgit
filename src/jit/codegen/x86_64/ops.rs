use arbitrary_int::u3;

use super::{
    instruction::{Immediate, Instruction},
    operand_encoding::{InstructionInput, MemoryBaseRegister, Offset, OperandEncoding},
};

fn imm_opcode(imm: Immediate, imm32_opcode: u8, imm8_opcode: u8) -> u8 {
    match imm {
        Immediate::Imm32(_) => return imm32_opcode,
        Immediate::Imm8(_) => return imm8_opcode,
        // Not supported
        Immediate::Imm64(_) => unreachable!(),
    }
}

#[derive(Clone, Debug, Copy)]
#[repr(u8)]
pub enum JumpOperator {
    /// JO
    JumpIfOverflow = 0,
    /// JNO
    JumpIfNotOverflow = 1,
    /// JB
    JumpIfBelow = 2,
    /// JNE
    JumpIfNotBelow = 3,
    /// JZ
    JumpIfZero = 4,
    /// JNZ
    JumpIfNotZero = 5,
    /// JBE
    JumpIfBelowOrEqual = 6,
    /// JA
    JumpIfAbove = 7,
    /// JS
    JumpIfSign = 8,
    /// JNS
    JumpIfNotSign = 9,
    /// JP
    JumpIfParity = 10,
    /// JNP
    JumpIfNotParity = 11,
    /// JL
    JumpIfLess = 12,
    /// JGE
    JumpIfGreaterOrEqual = 13,
    /// JLE
    JumpIfLessOrEqual = 14,
    /// JG
    JumpIfGreater = 15,
}

pub fn jump_op(jump: JumpOperator, op: OperandEncoding) -> Instruction {
    match op {
        OperandEncoding::Address(Offset::Immediate(Immediate::Imm8(_))) => {
            Instruction::new(InstructionInput::new(0x70 + jump as u8), op)
        }
        OperandEncoding::Address(Offset::Immediate(_)) => Instruction::new(
            InstructionInput::new(0x80 + jump as u8).with_two_byte_opcode_prefix(),
            op,
        ),
        _ => {
            unreachable!("{:?} is not a valid encoding type for {:?}", op, jump)
        }
    }
}

fn math_op(
    name: &'static str,
    op: OperandEncoding,
    opcode_imm: u8,
    opcode_mr: u8,
    opcode_extension: u8,
) -> Instruction {
    match op {
        OperandEncoding::Immediate(_, _) => Instruction::new(InstructionInput::new(opcode_imm), op),
        OperandEncoding::MemoryImmediate(_, imm) => Instruction::new(
            InstructionInput::new(imm_opcode(imm, 0x81, 0x83))
                .with_extension(u3::from(opcode_extension)),
            op,
        ),
        OperandEncoding::MemoryRegister(_, _) => {
            Instruction::new(InstructionInput::new(opcode_mr), op)
        }
        OperandEncoding::RegisterMemory(_, _) => {
            Instruction::new(InstructionInput::new(opcode_mr + 2), op)
        }
        _ => {
            unreachable!("{:?} is not a valid encoding type for {}", op, name)
        }
    }
}

pub fn add(op: OperandEncoding) -> Instruction {
    math_op("add", op, 0x4, 0x1, 0x0)
}

pub fn or(op: OperandEncoding) -> Instruction {
    math_op("or", op, 0xC, 0x9, 0x1)
}

/// Add with Carry
pub fn addc(op: OperandEncoding) -> Instruction {
    math_op("addc", op, 0x14, 0x11, 0x2)
}

/// Integer subtraction with borrow
pub fn sbb(op: OperandEncoding) -> Instruction {
    math_op("sbb", op, 0x1C, 0x19, 0x3)
}

pub fn and(op: OperandEncoding) -> Instruction {
    math_op("and", op, 0x24, 0x21, 0x4)
}

pub fn sub(op: OperandEncoding) -> Instruction {
    math_op("sub", op, 0x2C, 0x29, 0x5)
}

pub fn xor(op: OperandEncoding) -> Instruction {
    math_op("xor", op, 0x34, 0x31, 0x6)
}

pub fn cmp(op: OperandEncoding) -> Instruction {
    math_op("cmp", op, 0x3C, 0x39, 0x7)
}

pub fn mov(op: OperandEncoding) -> Instruction {
    match op {
        /* == Optimizations begin == */
        // XOR(reg, reg) is more optimized than MOV(reg, 0) since it's pipelined
        // this won't work if the register is a memory access (i.e. has a displacement or is a scaled index)
        // if we are doing a read from memory then it's more efficient to just move 0 into that memory location (to avoid a read & a write and just do a write)
        OperandEncoding::OpcodeImmediate(reg, imm)
        | OperandEncoding::MemoryImmediate(MemoryBaseRegister::Register(reg), imm)
            if imm.is_zero() =>
        {
            xor(OperandEncoding::MemoryRegister(
                MemoryBaseRegister::Register(reg.clone()),
                reg,
            ))
        }
        /* == Optimizations end == */
        OperandEncoding::Immediate(_, _) => {
            unreachable!("{:?} is not a valid encoding type for mov", op)
        }
        OperandEncoding::MemoryImmediate(_, _) => Instruction::new(InstructionInput::new(0xC7), op),
        OperandEncoding::MemoryRegister(_, _) => Instruction::new(InstructionInput::new(0x89), op),
        OperandEncoding::RegisterMemory(_, _) => Instruction::new(InstructionInput::new(0x8B), op),
        OperandEncoding::OpcodeImmediate(_, _) => Instruction::new(InstructionInput::new(0xB8), op),
        _ => {
            unreachable!("{:?} is not a valid encoding type for {}", op, "mov")
        }
    }
}
