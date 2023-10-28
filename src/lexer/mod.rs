use thiserror::Error;

pub mod lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum LexerTokenKind {
    // `>`: Increment the `data pointer` by one
    Increment,
    // `<`: Decrement the `data pointer` by one
    Decrement,
    
    // `+`: Increment the byte at the `data pointer` by one
    DerefIncrement,
    // `-`: Decrement the byte at the `data pointer` by one
    DerefDecrement,

    // `.`: Write the byte at the `data pointer` to the `output device`
    Write,
    // `.`: Read the next byte from the `output device` and write it to the `data pointer`
    Read,

    // `[`: If the byte at the `data pointer` is zero, then jump the `instruction pointer` forward to the instruction after the matching `]`
    JumpStart,
    // `]`: If the byte at the `data pointer` is non-zero then jump the `instruction pointer` back to the instruction after the matching `[`
    JumpEnd,

    // End of file: no more tokens left
    EOF,

    // Comment every other character
    Comment(String),
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("IO Error")]
    FileIO(
        #[from]
        std::io::Error,
    ),

    #[error("Can't find other symbol ({other:}) for {symbol:}")]
    MisbalancedSymbol { symbol: char, other: char },
}


