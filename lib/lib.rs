extern crate proc_macro;
use clap::Id;
use proc_macro::TokenStream;
use proc_macro2::Delimiter;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{bracketed, parse_macro_input, token, Block, Expr, Ident, Result, Token};

#[derive(Debug)]
struct AsmStatements {
    pub statements: Vec<AsmStatement>,
    pub labels: Vec<Ident>,
}

#[derive(Debug)]
enum Operand {
    /// Everything that doesn't fit
    Expr(Expr),
    /// RSP/others
    Ident(Ident),
    /// [SPL] or whatever, is all that we'll support for now
    MemoryRegister(Ident),
}

#[derive(Debug)]
enum AsmStatement {
    Label(Ident),
    Instruction(Ident, Vec<Operand>),
    Jump(Ident, Ident),
    Block(Expr),
}

fn parse_operands(input: ParseStream) -> Result<Vec<Operand>> {
    let mut result = vec![];

    loop {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            let label: Ident = input.parse()?;
            result.push(Operand::Ident(label))
        } else if lookahead.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            result.push(Operand::MemoryRegister(content.parse()?));
        } else {
            result.push(Operand::Expr(input.parse()?))
        }

        if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
            break Ok(result);
        } else {
            input.parse::<Token![,]>()?;
        }
    }
}

impl Parse for AsmStatements {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut statements = vec![];
        let mut labels = vec![];

        while !input.is_empty() {
            let mut lookahead = input.lookahead1();
            if lookahead.peek(Ident) {
                let label: Ident = input.parse()?;
                lookahead = input.lookahead1();
                if lookahead.peek(Token![:]) {
                    input.parse::<Token![:]>()?;
                    labels.push(label.clone());
                    statements.push(AsmStatement::Label(label));
                } else if label.to_string().starts_with("jump") {
                    statements.push(AsmStatement::Jump(label, input.parse()?));
                    input.parse::<Token![;]>()?;
                } else {
                    // we have an instruction
                    statements.push(AsmStatement::Instruction(label, parse_operands(input)?))
                }
            } else if lookahead.peek(Token![$]) {
                input.parse::<Token![$]>()?;
                statements.push(AsmStatement::Block(input.parse()?));
                input.parse::<Token![;]>()?;
            } else {
                return Err(lookahead.error());
            }
        }

        Ok(AsmStatements { statements, labels })
    }
}

fn encode_operands(operands: Vec<Operand>) -> proc_macro2::TokenStream {
    match &operands[0..] {
        [Operand::Ident(ident), Operand::Expr(expr)] => quote! {
            OperandEncoding::MemoryImmediate(MemoryBaseRegister::Register(registers::#ident), Immediate::Imm8(#expr))
        },
        [Operand::MemoryRegister(ident), Operand::Expr(expr)] => quote! {
            OperandEncoding::MemoryImmediate(MemoryBaseRegister::DisplacementOnly(registers::#ident, Displacement::ZeroByteDisplacement), Immediate::Imm8(#expr))
        },
        _ => todo!("{:#?}", operands),
    }
}

#[proc_macro]
pub fn asm_x86(stream: TokenStream) -> TokenStream {
    let input = parse_macro_input!(stream as AsmStatements);

    input
        .labels
        .into_iter()
        .map(|ident: Ident| {
            quote! {
                let mut #ident = Label::new(self.instructions.clone());
            }
            .into()
        })
        .chain(
            input
                .statements
                .into_iter()
                .map(|statement: AsmStatement| -> TokenStream {
                    match statement {
                        AsmStatement::Label(ident) => quote! {
                            #ident.current_offset_as_label();
                        },
                        AsmStatement::Instruction(ident, operands) => {
                            let tree = encode_operands(operands);
                            quote! {
                                self.push(ops::#ident(#tree));
                            }
                        }
                        AsmStatement::Jump(jump_type, jump_label) => {
                            let mut jump_op_text = jump_type.to_string();
                            jump_op_text.get_mut(0..1).unwrap().make_ascii_uppercase();
                            let jump_type = Ident::new(&jump_op_text, jump_type.span());

                            quote! {
                                #jump_label.add_jump(JumpOperator::#jump_type);
                            }
                        }
                        AsmStatement::Block(block) => quote! {
                            #block;
                        },
                    }
                    .into()
                }),
        )
        .collect()
}
