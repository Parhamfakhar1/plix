use std::fmt;
use crate::utils::position::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    
    // Keywords
    Mut,
    Const,
    Def,
    Class,
    If,
    Else,
    Return,
    True,
    False,
    Null,
    Undefined,
    
    // Operators
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Bang,
    Tilde,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    
    // Keywords
    Enum,
    Arrow,
    Elif,
    Comment,
    Newline,
    
    // Delimiters
    LParen,  // (
    RParen,  // )
    LBrace,  // {
    RBrace,  // }
    Comma,   // ,
    Semicolon, // ;
    Colon,   // :
    Question, // ?
    
    // End of file
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier(s) => write!(f, "identifier '{}'", s),
            TokenKind::IntegerLiteral(n) => write!(f, "integer {}", n),
            TokenKind::StringLiteral(s) => write!(f, "string '{}'", s),
            TokenKind::Mut => write!(f, "'mut'"),
            TokenKind::Const => write!(f, "'const'"),
            TokenKind::Def => write!(f, "'def'"),
            TokenKind::Class => write!(f, "'class'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Else => write!(f, "'else'"),
            TokenKind::Return => write!(f, "'return'"),
            TokenKind::True => write!(f, "'true'"),
            TokenKind::False => write!(f, "'false'"),
            TokenKind::Null => write!(f, "'null'"),
            TokenKind::Undefined => write!(f, "'undefined'"),
            TokenKind::Assign => write!(f, "'='"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Equal => write!(f, "'=='"),
            TokenKind::NotEqual => write!(f, "'!='"),
            TokenKind::Less => write!(f, "'<'"),
            TokenKind::LessEqual => write!(f, "'<='"),
            TokenKind::Greater => write!(f, "'>'"),
            TokenKind::GreaterEqual => write!(f, "'>='"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::PlusAssign => write!(f, "'+='"),
            TokenKind::MinusAssign => write!(f, "'-='"),
            TokenKind::StarAssign => write!(f, "'*='"),
            TokenKind::SlashAssign => write!(f, "'/='"),
            TokenKind::PercentAssign => write!(f, "'%='"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Semicolon => write!(f, "';'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::Enum => write!(f, "'enum'"),
            TokenKind::Arrow => write!(f, "'->'"),
            TokenKind::Elif => write!(f, "'elif'"),
            TokenKind::Comment => write!(f, "'#'"),
            TokenKind::Newline => write!(f, "'\\n'"),
            TokenKind::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, lexeme: String) -> Self {
        Self { kind, span, lexeme }
    }
    
    pub fn eof(position: crate::utils::position::Position) -> Self {
        Self {
            kind: TokenKind::EOF,
            span: Span::new(position, position),
            lexeme: String::new(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {} ('{}')", self.kind, self.span, self.lexeme)
    }
}
