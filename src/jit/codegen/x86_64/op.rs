use arbitrary_int::u3;

use super::{
    instruction::{EncodedRegister, Immediate, Instruction, RexPrefixEncoding, Prefix, TwoByteOpcode, ScaledIndexByte},
    operand::{Operand, Register, MemoryBaseRegister, ImmediateRegister, ScaledIndex}, codegen::Opcode, value_fits_in_i32, value_fits_in_i8,
};

/// The main reasons for this kind of design is the following observations
/// - there are many variants of the same opcode (OR/ADD/MOV/...) that just differ in their encoding
///   i.e. MOV r64, imm64 that uses OI encoding vs MOV r/m64, imm32 that uses MI encoding
/// - we want a 2-way system (assembly & disassembly) so we want the knowledge of which mnemonics map anyways.
/// - we want to keep it low level without a lot of abstraction
/// 
/// Thus you can use it something like this;
/// emit_instruction(op::mov(OperandEncoding::OpcodeImmediate(Register::RCX), Immediate::Imm64(0)))
/// OR
/// emit_instruction(op::mov(OperandEncoding::MemoryImmediate(MemoryBaseRegister { ... }, Immediate::Imm32(0))))

#[derive(Clone, Debug)]
enum OperandEncoding {
    /// "I", Operand 1 = AL/AX/EAX/RAX, Operand 2 = imm8/16/32
    /// has no MOD/RM byte, has separate opcode for AL/AX
    /// and can use REX.W to 64 bit extend to RAX.
    Immediate(ImmediateRegister, Immediate),

    /// "MI", Operand 1 = ModRM:r/m (r, w), Operand 2 = imm8/16/32
    MemoryImmediate(MemoryBaseRegister, Immediate),

    /// "MR", Operand 1 = ModRM:r/m (r, w), Operand 2 = ModRM:reg (r)
    MemoryRegister(MemoryBaseRegister, Register),

    /// "RM", Operand 1 = ModRM:reg (r), Operand 2 = ModRM:r/m (r, w)
    RegisterMemory(Register, MemoryBaseRegister),

    /// "OI", Operand 1 = opcode + rd (w), Operand 2 = imm8/16/32/64
    OpcodeImmediate(Register, Immediate),
    // TODO: FD & TD
}

struct InstructionInput {
    pub mandatory_prefix: Option<u8>,
    pub primary_opcode: u8,
    pub secondary_opcode: Option<u8>,
    pub opcode_extension: Option<u3>,
}

impl InstructionInput {
    pub fn new_primary_with_extension(primary_opcode: u8, opcode_extension: u3) {
        InstructionInput {
            mandatory_prefix: None,
            primary_opcode,
            secondary_opcode: None,
            opcode_extension: Some(opcode_extension),
        }
    }

    pub fn new_primary(primary_opcode: u8) {
        InstructionInput {
            mandatory_prefix: None,
            primary_opcode,
            secondary_opcode: None,
            opcode_extension: None,
        }
    }

    pub fn new_secondary(primary_opcode: u8, secondary_opcode: u8) {
        InstructionInput {
            mandatory_prefix: None,
            primary_opcode,
            secondary_opcode: Some(secondary_opcode),
            opcode_extension: None,
        }
    }
    
    pub fn new_primary_with_mandatory(primary_opcode: u8, mandatory_prefix: u8) {
        InstructionInput {
            mandatory_prefix: Some(mandatory_prefix),
            primary_opcode,
            secondary_opcode: None,
            opcode_extension: None,
        }
    }

    pub fn new_secondary(primary_opcode: u8, secondary_opcode: u8, mandatory_prefix: u8) {
        InstructionInput {
            mandatory_prefix: Some(mandatory_prefix),
            primary_opcode,
            secondary_opcode: Some(secondary_opcode),
            opcode_extension: None,
        }
    }
}

fn get_props_from_register(reg: Register) -> (EncodedRegister, EncodedRegister, Option<RexPrefixEncoding>, Option<ScaledIndexByte>) {
    let mut result = RexPrefixEncoding::Base | RexPrefixEncoding::W;

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
}

impl Instruction {
    pub fn new(input: InstructionInput, encoding: OperandEncoding) -> Instruction {
        let two_byte_opcode = if let Some(_) = input.secondary_opcode { Some(TwoByteOpcode::Value) } else { None };

        match encoding {
            OperandEncoding::OpcodeImmediate(reg, mut imm) => {
                let (encoded, rex) = EncodedRegister::from_register(reg);
                if rex.contains(RexPrefixEncoding::W) {
                    // if we are using a wide register then we should coerce our immediate
                    imm = match imm {
                        Immediate::Imm8(i) => Immediate::Imm8(i),
                        Immediate::Imm32(i) => Immediate::Imm32(i),
                        Immediate::Imm64(i) => Immediate::Imm64(i),
                    }
                } else if let Immediate::Imm64(_) = imm {
                    // i.e. doing MOV AX, imm64 is not valid
                    unreachable!("We are attempting to write a 64 bit value into a smaller register, this is not supported");
                }

                Instruction {
                    prefix: Prefix {
                        rex: Some(rex),
                        mandatory_prefix: input.mandatory_prefix,
                        two_byte_opcode,
                    },
                    // we can store the register with the primary opcode if it's OpcodeImmediate encoding
                    primary_opcode: input.primary_opcode | encoded.raw_value().extract_u8(),
                    secondary_opcode: input.secondary_opcode,
                    // no mod_rm/sib/displacement allowed
                    mod_rm: None,
                    sib: None,
                    displacement: None,
                    immediate: Some(imm),
                }
            }
            OperandEncoding::Immediate(reg, imm) => {
                // we don't actually encode the register since it's kinda stored in the opcode
                let rex = if reg == ImmediateRegister::RAX {
                    Some(RexPrefixEncoding::Base | RexPrefixEncoding::W)
                } else {
                    None
                };

                Instruction {
                    prefix: Prefix {
                        rex,
                        mandatory_prefix: input.mandatory_prefix,
                        two_byte_opcode,
                    },
                    // we can store the register with the primary opcode if it's OpcodeImmediate encoding
                    primary_opcode: input.primary_opcode,
                    secondary_opcode: input.secondary_opcode,
                    // no mod_rm/sib/displacement allowed
                    mod_rm: None,
                    sib: None,
                    displacement: None,
                    immediate: Some(imm),
                }
            },
            OperandEncoding::MemoryImmediate(mem_reg, imm) => {
                // we don't actually encode the register since it's kinda stored in the opcode
                let (encoded, rex) = EncodedRegister::from_register(Operand::MemoryBaseRegister(mem_reg));

                Instruction {
                    prefix: Prefix {
                        rex: Some(rex),
                        mandatory_prefix: input.mandatory_prefix,
                        two_byte_opcode,
                    },
                    // we can store the register with the primary opcode if it's OpcodeImmediate encoding
                    primary_opcode: input.primary_opcode,
                    secondary_opcode: input.secondary_opcode,
                    // no mod_rm/sib/displacement allowed
                    mod_rm: None,
                    sib: None,
                    displacement: None,
                    immediate: Some(imm),
                }
            },
            OperandEncoding::MemoryRegister() => todo!(),
            OperandEncoding::RegisterMemory(reg) => todo!(),
        }
    }
}

pub fn imm_opcode(imm: Immediate, imm32: u8, imm8: u8) -> u8 {
    match imm {
        Immediate::Imm32(_) => return imm32,
        Immediate::Imm8(_) => return imm8,
        // Not supported
        Immediate::Imm64(_) => unreachable!(),
    }
}

pub fn xor(op: OperandEncoding) -> Instruction {
    match op {
        OperandEncoding::Immediate(reg, _) => Instruction::new(InstructionInput::new_primary(0x34 + (reg as u8)), op),
        OperandEncoding::MemoryImmediate(_, imm) => Instruction::new(InstructionInput::new_primary(imm_opcode(imm, 0x81, 0x83)), op),
        OperandEncoding::MemoryRegister(_, _) => Instruction::new(InstructionInput::new_primary(0x31), op),
        OperandEncoding::RegisterMemory(_, _) => Instruction::new(InstructionInput::new_primary(0x33), op),
        OperandEncoding::OpcodeImmediate(_, _) => unreachable!("{:?} is not a valid encoding type for xor", op),
    }
}

pub fn mov(op: OperandEncoding) -> Instruction {
    match op {
        /* == Optimizations begin == */
        // XOR(reg, reg) is more optimized than MOV(reg, 0) since it's pipelined
        // this won't work if the register is a memory access (i.e. has a displacement or is a scaled index)
        // if we are doing a read from memory then it's more efficient to just move 0 into that memory location (to avoid a read & a write and just do a write)
        OperandEncoding::OpcodeImmediate(reg, Immediate::Imm8(0)) | OperandEncoding::OpcodeImmediate(reg, Immediate::Imm32(0)) | OperandEncoding::OpcodeImmediate(reg, Immediate::Imm64(0))
        | OperandEncoding::MemoryImmediate(MemoryBaseRegister::Register(reg), Immediate::Imm8(0)) | OperandEncoding::MemoryImmediate(MemoryBaseRegister::Register(reg), Immediate::Imm32(0)) | OperandEncoding::MemoryImmediate(MemoryBaseRegister::Register(reg), Immediate::Imm64(0)) =>
            xor(OperandEncoding::MemoryRegister(MemoryBaseRegister::Register(reg), reg)),
        /* == Optimizations end == */

        OperandEncoding::Immediate(_, _) => unreachable!("{:?} is not a valid encoding type for mov", op),
        OperandEncoding::MemoryImmediate(_, _) => Instruction::new(InstructionInput::new_primary(0xC7), op),
        OperandEncoding::MemoryRegister(_, _) => Instruction::new(InstructionInput::new_primary(0x89), op),
        OperandEncoding::RegisterMemory(_, _) => Instruction::new(InstructionInput::new_primary(0x8B), op),
        OperandEncoding::OpcodeImmediate(_, _) => Instruction::new(InstructionInput::new_primary(0xB8), op),
    }
}

pub fn encode(op: Opcode, dst: Operand, src: Operand) {
    // Optimizations
    if op == Opcode::Mov {
        if let (Operand::Register(reg), Operand::Immediate(imm)) = (dst, src) {
            if imm == 0 {
                // MOV(r, 0) = XOR(r, r) since it's better pipelined
                return encode(Opcode::Xor, dst, Operand::Register(reg));
            } else {
                // MOV(r, imm) = B8 + r since byte instruction no opcode
                if value_fits_in_i32(imm) {
                    // TODO: This is messy but we want to basically remove the :W since we are emitting a 32 bit imm
                    // self.emit8(
                    //     (RexPrefixEncoding::from_operand(reg) & !RexPrefixEncoding::W).as_u8(),
                    // );
                    // self.emit8(0xB8 | reg.encode_register());
                    // self.emit32(imm);
                } else {
                    // We emit .W to indicate it's a large 64 bit imm
                    // self.emit8(RexPrefixEncoding::from_operand(reg).as_u8());
                    // self.emit8(0xB8 | reg.encode_register());
                    // self.emit64(imm);
                }
                return;
            }
        }
        if let (Operand::Register(reg1), Operand::Register(reg2)) = (dst, src) {
            // MOV(r, r) always is a no-op
            if reg1 == reg2 {
                return;
            }
        }
    }

    let (primaryOpcode, opcodeExtension) = match (dst, src) {
        (Operand::Register(_, Some(_)), Operand::Register(_, None)) => match op {
            Opcode::Add => (0x01, None),
            Opcode::Sub => (0x29, None),
            Opcode::Mov => (0x89, None),
            Opcode::Cmp => (0x39, None),
            Opcode::Xor => todo!(),
        },
        (Operand::Register(_, None), Operand::Register(_, Some(_))) => match op {
            Opcode::Add => (0x03, None),
            Opcode::Sub => (0x2B, None),
            Opcode::Mov => (0x8B, None),
            Opcode::Cmp => (0x3B, None),
            Opcode::Xor => todo!(),
        },
        (Operand::Register(_, _), Operand::Immediate(imm)) => {
            if value_fits_in_i8(imm) {
                // imm8
                match op {
                    Opcode::Add => (0x83, Some(0)),
                    Opcode::Sub => (0x83, Some(5)),
                    Opcode::Mov => (0xC7, Some(0)),
                    Opcode::Cmp => todo!(),
                    Opcode::Xor => todo!(),
                }
            } else if value_fits_in_i32(imm) {
                // imm32
                match op {
                    Opcode::Add => (0x81, Some(0)),
                    Opcode::Sub => (0x81, Some(5)),
                    Opcode::Mov => (0xC7, Some(0)),
                    Opcode::Cmp => todo!(),
                    Opcode::Xor => todo!(),
                }
            } else {
                unreachable!()
            }
        }
        _ => unreachable!(),
    };
}
