use arbitrary_int::u3;

use super::{
    instruction::{
        AddressingMode, Displacement, Immediate, Instruction, ModRM, Prefix, RexPrefixEncoding,
        ScaledIndexByte, TwoByteOpcode,
    },
    registers::{self, Register, RegisterAccess},
};

#[derive(Clone, Debug)]
pub enum Operand {
    Register(Register),
    MemoryBaseRegister(MemoryBaseRegister),
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

#[derive(Clone, Debug)]
pub struct ScaledIndex {
    pub base: Option<Register>,
    pub index: Option<(Register, u8)>,
    pub displacement: Displacement,
}

// impl Register {
//     pub fn encode_register(&self) -> EncodedRegister {
//         // https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
//         // X.Reg, in this case our encoded register is just encoding the Y part of X.YYY
//         // we'll need to set REX flags X & B when using dest & src registers
//         return EncodedRegister::new_with_raw_value(u3::from((*self as u8) & 0x7));
//     }
// }

// impl EncodedRegister {
//     pub fn from_registers(
//         dst: Operand,
//         src: Operand,
//     ) -> ((EncodedRegister, EncodedRegister), RexPrefixEncoding) {
//         let rex = RexPrefixEncoding::from_operands(dst, src);
//         let (Operand::Register(dst, _), Operand::Register(src, _)) = (dst, src);
//         ((dst.encode_register(), src.encode_register()), rex)
//     }

//     pub fn from_register(dst: Operand) -> (EncodedRegister, RexPrefixEncoding) {
//         let rex = RexPrefixEncoding::from_operand(dst);
//         let Operand::Register(dst, _) = dst;
//         (dst.encode_register(), rex)
//     }
// }

impl AddressingMode {
    pub fn from_displacement(displacement: Option<Displacement>) -> AddressingMode {
        match displacement {
            Some(Displacement::ZeroByteDisplacement) => AddressingMode::ZeroByteDisplacement,
            Some(Displacement::OneByteDisplacement(_)) => AddressingMode::OneByteDisplacement,
            Some(Displacement::FourByteDisplacement(_)) => AddressingMode::FourByteDisplacement,
            None => AddressingMode::RegisterDirect,
        }
    }
}

impl RexPrefixEncoding {
    pub fn as_u8(&self) -> u8 {
        self.bits() as u8
    }
}

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
pub enum OperandEncoding {
    /// "I", Operand 1 = AL/AX/EAX/RAX, Operand 2 = imm8/16/32
    /// has no MOD/RM byte, has separate opcode for AL/AX
    /// and can use REX.W to 64 bit extend to RAX.
    Immediate(RegisterAccess, Immediate),

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

pub struct InstructionInput {
    pub mandatory_prefix: Option<u8>,
    pub primary_opcode: u8,
    pub secondary_opcode: Option<u8>,
    pub opcode_extension: Option<u3>,
}

impl InstructionInput {
    pub fn new(primary_opcode: u8) -> InstructionInput {
        InstructionInput {
            mandatory_prefix: None,
            primary_opcode,
            secondary_opcode: None,
            opcode_extension: None,
        }
    }

    pub fn with_extension(mut self, opcode_extension: u3) -> InstructionInput {
        self.opcode_extension = Some(opcode_extension);
        self
    }

    pub fn with_secondary(mut self, secondary_opcode: u8) -> InstructionInput {
        self.secondary_opcode = Some(secondary_opcode);
        self
    }
}

impl Instruction {
    fn set_rex(&mut self, rex: RexPrefixEncoding) {
        self.prefix.rex = Some(self.prefix.rex.unwrap_or(RexPrefixEncoding::Base) | rex);
    }

    fn set_modrm<T: FnOnce(ModRM) -> ModRM>(&mut self, func: T) {
        self.mod_rm = Some(func(self.mod_rm.unwrap_or(ModRM::default())));
    }

    fn encode_register(&mut self, register: Register, output_modrm: bool) {
        let (src, access) = register;
        if src.requires_rex_flag() {
            self.set_rex(RexPrefixEncoding::R);
        }

        // TODO: We need to set prefixes & other stuff to get access to the 8/16/32 bit variants
        // but for 64 bit we just need to append a .W to write to the 64 bit register
        // TODO: https://wiki.osdev.org/X86-64_Instruction_Encoding#Operand-size_and_address-size_override_prefix

        if access == RegisterAccess::LowEightBytes {
            // some instructions don't require this, in whcih case we'll clean this up when outputting the instruction
            // to the instruction stream, it's okay for us to output this regardless it's just an optimization (saving bytes)
            self.set_rex(RexPrefixEncoding::W);
        }

        if output_modrm {
            self.set_modrm(|modrm| modrm.with_register(src.as_u3()));
        } else if let Some(_) = self.mod_rm {
            panic!("Can't set mod_rm through opcode_extension bits if using an OpcodeImmediate encoding");
        }
    }

    fn encode_memregister(&mut self, mem_reg: MemoryBaseRegister) {
        match mem_reg {
            MemoryBaseRegister::Register((reg, _)) => {
                if reg.requires_rex_flag() {
                    self.set_rex(RexPrefixEncoding::B);
                }

                self.set_modrm(|modrm| modrm.with_register_memory(reg.as_u3()));
            }
            MemoryBaseRegister::DisplacementOnly((reg, _), mut displacement) => {
                if reg.requires_rex_flag() {
                    self.set_rex(RexPrefixEncoding::B);
                }

                if (reg == registers::BP || reg == registers::R13)
                    && displacement == Displacement::ZeroByteDisplacement
                {
                    // in x86 this causes us to use RIP/EIP instead if we are in Mod == 00 (ZeroByteDisplacement)
                    // so to still output BP/R13 we need to actually use Mod == 01 (OneByteDisplacement) which doesn't
                    // have this fallthrough behaviour.  By setting displacement here it'll set the right addressing mode below
                    displacement = Displacement::OneByteDisplacement(0);
                }

                self.set_modrm(|modrm| {
                    modrm
                        // @NOTE: The override in the ZeroByteDisplacement case above
                        .with_addressing_mode(AddressingMode::from_displacement(Some(displacement)))
                        .with_register_memory(reg.as_u3())
                });

                self.displacement = Some(displacement);
            }
            MemoryBaseRegister::ScaledIndex(ScaledIndex {
                base,
                index,
                mut displacement,
            }) => {
                let mut sib = ScaledIndexByte::default();

                if let Some((base, base_access)) = base {
                    if base == registers::BP || base == registers::R13 {
                        // these are special registers that cause the SIB byte to be outputted
                        // outside of 0 byte displacement values they are encoded into the SIB
                        // but for 0 byte displacement values it means *no* base so if we have stated
                        // we want an explicit base of BP/R13 & we are 0 byte displacement we need to change that
                        if displacement == Displacement::ZeroByteDisplacement {
                            displacement = Displacement::OneByteDisplacement(0);
                        }
                    }

                    if base.requires_rex_flag() {
                        self.set_rex(RexPrefixEncoding::B);
                    }

                    if base_access == RegisterAccess::LowEightBytes {
                        self.set_rex(RexPrefixEncoding::W);
                    }

                    sib = sib.with_base(base.as_u3());
                    self.set_modrm(|modrm| {
                        modrm
                            // enable SIB (this register or R13 if REX.W is enabled will cause it to read the SIB byte)
                            .with_register_memory(registers::SP.as_u3())
                            .with_addressing_mode(AddressingMode::from_displacement(Some(
                                displacement,
                            )))
                    })
                } else {
                    // NOTE: This is a special register to designate there is no base
                    //       we can also use R13 to designate this.
                    sib = sib.with_base(registers::BP.as_u3());
                    // This only works if the mode in the instruction is 00 (for zero displacement)
                    // but the displacement is actually 4 bytes (no version for 1 byte) so we coerce
                    displacement = displacement.coerce_to_fourbytes();
                    self.set_modrm(|modrm| {
                        modrm
                            // enable SIB (this register or R13 if REX.W is enabled will cause it to read the SIB byte)
                            .with_register_memory(registers::SP.as_u3())
                            .with_addressing_mode(AddressingMode::ZeroByteDisplacement)
                    })
                }

                if let Some(((index, index_access), _)) = index {
                    if index == registers::SP {
                        panic!("SP is not a valid register to use as an index");
                    }

                    if index.requires_rex_flag() {
                        self.set_rex(RexPrefixEncoding::X);
                    }

                    if index_access == RegisterAccess::LowEightBytes {
                        self.set_rex(RexPrefixEncoding::W);
                    }
                } else {
                    // SP is a special register to designate that there is no index
                    sib = sib.with_index(registers::SP.as_u3());
                }

                self.sib = Some(sib);
                self.displacement = Some(displacement);
            }
        }
    }

    fn encode_immediate(&mut self, imm: Immediate) {
        // TODO: Coerce / validate immediate based on access pattern
        self.immediate = Some(imm);
    }

    pub fn new(input: InstructionInput, encoding: OperandEncoding) -> Instruction {
        let two_byte_opcode = if let Some(_) = input.secondary_opcode {
            Some(TwoByteOpcode::Value)
        } else {
            None
        };

        let mut instruction = Instruction {
            prefix: Prefix {
                rex: None,
                mandatory_prefix: input.mandatory_prefix,
                two_byte_opcode,
            },
            primary_opcode: input.primary_opcode,
            secondary_opcode: input.secondary_opcode,
            mod_rm: ModRM::new_if_opcode(input.opcode_extension),
            sib: None,
            displacement: None,
            immediate: None,
        };

        match encoding {
            OperandEncoding::OpcodeImmediate((dest_reg, access), imm) => {
                // we can store the register with the primary opcode if it's OpcodeImmediate encoding
                instruction.primary_opcode |= dest_reg.as_u3().value();
                instruction.encode_register((dest_reg, access), false);
                instruction.encode_immediate(imm);
            }
            OperandEncoding::Immediate(access, imm) => {
                // we don't actually encode the register since it's stored in the opcode
                let opcode_offset = match access {
                    RegisterAccess::LowByte => 0,
                    RegisterAccess::HighByte => panic!(
                        "We don't support access to AH when using OperandEncoding::Immediate"
                    ),
                    // AX, EAX, RAX
                    RegisterAccess::LowTwoBytes
                    | RegisterAccess::LowFourBytes
                    | RegisterAccess::LowEightBytes => 1,
                };

                instruction.primary_opcode += opcode_offset;
                instruction.encode_register((registers::A, access), false);
                instruction.encode_immediate(imm);
            }
            OperandEncoding::MemoryImmediate(mem_reg, imm) => {
                instruction.encode_memregister(mem_reg);
                instruction.encode_immediate(imm);
            }
            // MR vs RM only matters for the opcode (and for what is dst/src) but from the point of encoding there is only 1 mem_reg & 1 reg either way
            OperandEncoding::MemoryRegister(mem_reg, reg)
            | OperandEncoding::RegisterMemory(reg, mem_reg) => {
                instruction.encode_memregister(mem_reg);
                instruction.encode_register(reg, true);
            }
        }

        instruction
    }
}
