use thiserror::Error;
use crate::utils::position::{Position, Span};

#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error("Unexpected character '{character}' at {position}")]
    UnexpectedCharacter {
        character: char,
        position: Position,
    },
    
    #[error("Unterminated string literal at {position}")]
    UnterminatedString {
        position: Position,
    },
    
    #[error("Unterminated comment at {position}")]
    UnterminatedComment {
        position: Position,
    },
    
    #[error("Invalid number format '{text}' at {position}")]
    InvalidNumberFormat {
        text: String,
        position: Position,
    },
    
    #[error("Unknown escape sequence '\\{character}' at {position}")]
    UnknownEscapeSequence {
        character: char,
        position: Position,
    },
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum CompilerError {
    #[error("Lexer error: {source}")]
    Lexer {
        #[from]
        source: LexerError,
    },
    
    #[error("Parser error at {span}")]
    Parser {
        span: Span,
        message: String,
    },
}

pub type CompilerResult<T> = Result<T, CompilerError>;