use arbitrary_int::u3;

use super::{
    instruction::{Immediate, Instruction},
    operand_encoding::{InstructionInput, MemoryBaseRegister, OperandEncoding},
};

fn imm_opcode(imm: Immediate, imm32_opcode: u8, imm8_opcode: u8) -> u8 {
    match imm {
        Immediate::Imm32(_) => return imm32_opcode,
        Immediate::Imm8(_) => return imm8_opcode,
        // Not supported
        Immediate::Imm64(_) => unreachable!(),
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
        OperandEncoding::OpcodeImmediate(_, _) => {
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
    }
}

// pub fn encode(op: Opcode, dst: Operand, src: Operand) {
//     // Optimizations
//     if op == Opcode::Mov {
//         if let (Operand::Register(reg), Operand::Immediate(imm)) = (dst, src) {
//             if imm == 0 {
//                 // MOV(r, 0) = XOR(r, r) since it's better pipelined
//                 return encode(Opcode::Xor, dst, Operand::Register(reg));
//             } else {
//                 // MOV(r, imm) = B8 + r since byte instruction no opcode
//                 if value_fits_in_i32(imm) {
//                     // TODO: This is messy but we want to basically remove the :W since we are emitting a 32 bit imm
//                     // self.emit8(
//                     //     (RexPrefixEncoding::from_operand(reg) & !RexPrefixEncoding::W).as_u8(),
//                     // );
//                     // self.emit8(0xB8 | reg.encode_register());
//                     // self.emit32(imm);
//                 } else {
//                     // We emit .W to indicate it's a large 64 bit imm
//                     // self.emit8(RexPrefixEncoding::from_operand(reg).as_u8());
//                     // self.emit8(0xB8 | reg.encode_register());
//                     // self.emit64(imm);
//                 }
//                 return;
//             }
//         }
//         if let (Operand::Register(reg1), Operand::Register(reg2)) = (dst, src) {
//             // MOV(r, r) always is a no-op
//             if reg1 == reg2 {
//                 return;
//             }
//         }
//     }

//     let (primaryOpcode, opcodeExtension) = match (dst, src) {
//         // (Operand::Register(_, Some(_)), Operand::Register(_, None)) => match op {
//         //     Opcode::Add => (0x01, None),
//         //     Opcode::Sub => (0x29, None),
//         //     Opcode::Mov => (0x89, None),
//         //     Opcode::Cmp => (0x39, None),
//         //     Opcode::Xor => todo!(),
//         // },
//         // (Operand::Register(_, None), Operand::Register(_, Some(_))) => match op {
//         //     Opcode::Add => (0x03, None),
//         //     Opcode::Sub => (0x2B, None),
//         //     Opcode::Mov => (0x8B, None),
//         //     Opcode::Cmp => (0x3B, None),
//         //     Opcode::Xor => todo!(),
//         // },
//         // (Operand::Register(_, _), Operand::Immediate(imm)) => {
//         //     if value_fits_in_i8(imm) {
//         //         // imm8
//         //         match op {
//         //             Opcode::Add => (0x83, Some(0)),
//         //             Opcode::Sub => (0x83, Some(5)),
//         //             Opcode::Mov => (0xC7, Some(0)),
//         //             Opcode::Cmp => todo!(),
//         //             Opcode::Xor => todo!(),
//         //         }
//         //     } else if value_fits_in_i32(imm) {
//         //         // imm32
//         //         match op {
//         //             Opcode::Add => (0x81, Some(0)),
//         //             Opcode::Sub => (0x81, Some(5)),
//         //             Opcode::Mov => (0xC7, Some(0)),
//         //             Opcode::Cmp => todo!(),
//         //             Opcode::Xor => todo!(),
//         //         }
//         //     } else {
//         //         unreachable!()
//         //     }
//         // }
//         _ => unreachable!(),
//     };
// }
