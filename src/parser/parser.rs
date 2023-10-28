use std::iter::Peekable;

use crate::lexer::LexerTokenKind;

use super::{Program, BasicBlock, AstKind};

pub struct Parser<'a> {
    tokens: Peekable<std::slice::Iter<'a, LexerTokenKind>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<LexerTokenKind>) -> Parser {
        Parser {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse_block(&mut self) -> BasicBlock {
        let mut instructions = vec![];

        while let Some(token) = self.tokens.next() {
            instructions.push(match *token {
                LexerTokenKind::Increment => AstKind::ShiftDataPointer(1),
                LexerTokenKind::Decrement => AstKind::ShiftDataPointer(-1),
                LexerTokenKind::DerefIncrement => AstKind::DerefIncrement(1),
                LexerTokenKind::DerefDecrement => AstKind::DerefDecrement(1),
                LexerTokenKind::Write => AstKind::Write(1),
                LexerTokenKind::Read => AstKind::Read(1),
                LexerTokenKind::JumpStart => AstKind::Loop(self.parse_block()),
                // the loop has ended so we can break
                LexerTokenKind::JumpEnd => break,
                // technically this would be an "error"
                // but since we already catch this in the lexer I'm not going to handle it here
                // just so that parse_program = parse_block for simplicity
                LexerTokenKind::EOF => break,
                // we aren't outputting comments to AST
                // at-least for now (for the sake of an unparser maybe we'll do it later...)
                LexerTokenKind::Comment(_) => continue,
            })
        }

        BasicBlock { instructions }
    }

    pub fn parse_program(&mut self) -> Program {
        self.parse_block()
    }

}