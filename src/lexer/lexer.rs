use colored::Colorize;

use super::{LexerTokenKind, LexerError};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    /** Human Readable positions in file */
    pub cur_line: usize,
    pub cur_col: usize,

    /** 'raw' format / offset within the file (in terms of 'codepoints') */
    pub codepoint_offset: usize,

    chars: std::iter::Peekable<std::str::Chars<'a>>,
    balancing_state: i32,
}


impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            cur_col: 1,
            cur_line: 1,

            codepoint_offset: 0,

            chars: chars.chars().peekable(),
            balancing_state: 0,
        }
    }

    fn transform_to_type(&mut self, c: char) -> Result<LexerTokenKind, LexerError> {
        match c {
            '>' => Ok(LexerTokenKind::Increment),
            '<' => Ok(LexerTokenKind::Decrement),
            '+' => Ok(LexerTokenKind::DerefIncrement),
            '-' => Ok(LexerTokenKind::DerefDecrement),
            '.' => Ok(LexerTokenKind::Write),
            ',' => Ok(LexerTokenKind::Read),
            '[' => {
                self.balancing_state += 1;
                Ok(LexerTokenKind::JumpStart)
            },
            ']' => {
                if self.balancing_state >= 1 {
                    self.balancing_state -= 1;
                    Ok(LexerTokenKind::JumpEnd)
                } else {
                    Err(LexerError::MisbalancedSymbol { symbol: ']', other: '[' })
                }
            },
            c => {
                // Simplify the comment stream down to strings
                let mut comment = String::from(c);
                loop {
                    match self.chars.peek() {
                        Some('>') | Some('<') | Some('+') | Some('-') | Some('.') | Some(',') | Some('[') | Some(']') => break,
                        Some(_) => comment.push(self.chars.next().unwrap()),
                        None => break,
                    }
                }

                Ok(LexerTokenKind::Comment(comment))
            },
        }
    }

    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.cur_col += 1;
                if c == '\n' {
                    self.cur_line += 1;
                    self.cur_col = 1;
                }
                self.codepoint_offset += 1;
                Some(c)
            }
            None => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.consume_char();
        }
    }

    pub fn next_token(&mut self) -> Result<LexerTokenKind, LexerError> {
        self.skip_whitespace();

        if let Some(c) = self.consume_char() {
            self.transform_to_type(c)
        } else if self.balancing_state > 0 {
            Err(LexerError::MisbalancedSymbol { symbol: '[', other: ']' })
        } else {
            Ok(LexerTokenKind::EOF)
        }
    }

    pub fn collect_results(&mut self) -> Result<Vec<LexerTokenKind>, LexerError> {
        let mut v = vec![];
        loop {
            match self.next_token() {
                Ok(LexerTokenKind::EOF) => return Ok(v),
                Err(e) => return Err(e),
                Ok(tok) => v.push(tok),
            }
        }
    }

    pub fn collect(&mut self) -> Vec<LexerTokenKind> {
        let mut v = vec![];
        loop {
            match self.next_token() {
                Ok(LexerTokenKind::EOF) => break v,
                Err(e) => eprintln!("{0:}: {1:}", "Error".red(), e),
                Ok(x) => v.push(x),
            }
        }
    }
}

