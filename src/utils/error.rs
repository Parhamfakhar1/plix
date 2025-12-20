use crate::utils::position::{Position, Span};
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error("Unexpected character '{character}' at {position}")]
    UnexpectedCharacter { character: char, position: Position },
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum CompilerError {
    #[error("Lexer error: {source}")]
    Lexer {
        #[from]
        source: LexerError,
    },

    #[error("Parser error at {span}")]
    Parser { span: Span, message: String },
}

pub type CompilerResult<T> = Result<T, CompilerError>;
