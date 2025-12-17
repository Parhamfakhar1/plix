use std::fmt;
use crate::utils::position::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(f64),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Null,
    Undefined,
    
    Def,
    Const,
    Mut,
    Class,
    Enum,
    Data,
    If,
    Elif,
    Else,
    For,
    While,
    Match,
    Case,
    Async,
    Await,
    Import,
    From,
    Return,
    True,
    False,
    
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Bang,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Amp,
    Pipe,
    Tilde,
    Shl,
    Shr,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Question,
    DoubleColon,
    Arrow,
    Range,
    Ellipsis,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Newline,
    Indent,
    Dedent,
    EOF,
}


impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier(s) => write!(f, "identifier '{}'", s),
            TokenKind::StringLiteral(s) => write!(f, "string '{}'", s),
            TokenKind::NumberLiteral(n) => write!(f, "number {}", n),
            TokenKind::IntegerLiteral(i) => write!(f, "integer {}", i),
            TokenKind::BooleanLiteral(b) => write!(f, "boolean {}", b),
            TokenKind::Null => write!(f, "null"),
            TokenKind::Undefined => write!(f, "undefined"),
            TokenKind::Def => write!(f, "'def'"),
            TokenKind::Const => write!(f, "'const'"),
            TokenKind::Mut => write!(f, "'mut'"),
            TokenKind::Class => write!(f, "'class'"),
            TokenKind::Enum => write!(f, "'enum'"),
            TokenKind::Data => write!(f, "'data'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Elif => write!(f, "'elif'"),
            TokenKind::Else => write!(f, "'else'"),
            TokenKind::For => write!(f, "'for'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::Match => write!(f, "'match'"),
            TokenKind::Case => write!(f, "'case'"),
            TokenKind::Async => write!(f, "'async'"),
            TokenKind::Await => write!(f, "'await'"),
            TokenKind::Import => write!(f, "'import'"),
            TokenKind::From => write!(f, "'from'"),
            TokenKind::Return => write!(f, "'return'"),
            TokenKind::True => write!(f, "'true'"),
            TokenKind::False => write!(f, "'false'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::Assign => write!(f, "'='"),
            TokenKind::PlusAssign => write!(f, "'+='"),
            TokenKind::MinusAssign => write!(f, "'-='"),
            TokenKind::StarAssign => write!(f, "'*='"),
            TokenKind::SlashAssign => write!(f, "'/='"),
            TokenKind::PercentAssign => write!(f, "'%='"),
            TokenKind::Equal => write!(f, "'=='"),
            TokenKind::NotEqual => write!(f, "'!='"),
            TokenKind::Less => write!(f, "'<'"),
            TokenKind::Greater => write!(f, "'>'"),
            TokenKind::LessEqual => write!(f, "'<='"),
            TokenKind::GreaterEqual => write!(f, "'>='"),
            TokenKind::And => write!(f, "'&&'"),
            TokenKind::Or => write!(f, "'||'"),
            TokenKind::Amp => write!(f, "'&'"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::Shl => write!(f, "'<<'"),
            TokenKind::Shr => write!(f, "'>>'"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Semicolon => write!(f, "';'"),
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::DoubleColon => write!(f, "'::'"),
            TokenKind::Arrow => write!(f, "'=>'"),
            TokenKind::Range => write!(f, "'..'"),
            TokenKind::Ellipsis => write!(f, "'...'"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBracket => write!(f, "'['"),
            TokenKind::RBracket => write!(f, "']'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Newline => write!(f, "newline"),
            TokenKind::Indent => write!(f, "indent"),
            TokenKind::Dedent => write!(f, "dedent"),
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
