use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::token::{Token, TokenKind};
use crate::utils::error::{LexerError, CompilerResult};
use crate::utils::position::{Position, Span};

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_pos: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            current_pos: Position::new(),
        }
    }
    
    pub fn tokenize(mut self) -> CompilerResult<Vec<Token>> {
        let mut tokens = Vec::new();
        
        while let Some(token) = self.next_token()? {
            tokens.push(token);
        }
        
        tokens.push(Token::eof(self.current_pos));
        Ok(tokens)
    }
    
    fn next_token(&mut self) -> CompilerResult<Option<Token>> {
        self.skip_whitespace();
        
        if self.is_at_end() {
            return Ok(None);
        }
        
        let start_pos = self.current_pos;
        let current_char = self.peek_char();
        
        let token = match current_char {
            'a'..='z' | 'A'..='Z' | '_' => self.scan_identifier(),
            '0'..='9' => self.scan_number(),
            '"' => self.scan_string()?,
            '=' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::Equal, "==")
                } else {
                    self.create_token(TokenKind::Assign, "=")
                }
            }
            '!' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::NotEqual, "!=")
                } else {
                    self.create_token(TokenKind::Bang, "!")
                }
            }
            '?' => {
                self.next_char();
                self.create_token(TokenKind::Question, "?")
            }
            '~' => {
                self.next_char();
                self.create_token(TokenKind::Tilde, "~")
            }
            '#' => {
                self.next_char();
                self.create_token(TokenKind::Comment, "#")
            }
            '\n' => {
                self.next_char();
                self.create_token(TokenKind::Newline, "\n")
            }
            '<' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::LessEqual, "<=")
                } else {
                    self.create_token(TokenKind::Less, "<")
                }
            }
            '>' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::GreaterEqual, ">=")
                } else {
                    self.create_token(TokenKind::Greater, ">")
                }
            }
            '+' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::PlusAssign, "+=")
                } else {
                    self.create_token(TokenKind::Plus, "+")
                }
            }
            '-' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::MinusAssign, "-=")
                } else {
                    self.create_token(TokenKind::Minus, "-")
                }
            }
            '*' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::StarAssign, "*=")
                } else {
                    self.create_token(TokenKind::Star, "*")
                }
            }
            '/' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::SlashAssign, "/=")
                } else {
                    self.create_token(TokenKind::Slash, "/")
                }
            }
            '%' => {
                self.next_char();
                if self.peek_char() == '=' {
                    self.next_char();
                    self.create_token(TokenKind::PercentAssign, "%=")
                } else {
                    self.create_token(TokenKind::Percent, "%")
                }
            }
            '(' => {
                self.next_char();
                self.create_token(TokenKind::LParen, "(")
            }
            ')' => {
                self.next_char();
                self.create_token(TokenKind::RParen, ")")
            }
            '{' => {
                self.next_char();
                self.create_token(TokenKind::LBrace, "{")
            }
            '}' => {
                self.next_char();
                self.create_token(TokenKind::RBrace, "}")
            }
            ',' => {
                self.next_char();
                self.create_token(TokenKind::Comma, ",")
            }
            ';' => {
                self.next_char();
                self.create_token(TokenKind::Semicolon, ";")
            }
            ':' => {
                self.next_char();
                if self.peek_char() == '>' {
                    self.next_char();
                    self.create_token(TokenKind::Arrow, "->")
                } else {
                    self.create_token(TokenKind::Colon, ":")
                }
            }
            ch => {
                return Err(LexerError::UnexpectedCharacter {
                    character: ch,
                    position: start_pos,
                }.into());
            }
        };
        
        Ok(Some(token))
    }
    
    fn scan_identifier(&mut self) -> Token {
        let start_pos = self.current_pos;
        let mut lexeme = String::new();
        
        while let Some(ch) = self.peek_char_option() {
            if ch.is_alphanumeric() || ch == '_' {
                lexeme.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        
        let kind = match lexeme.as_str() {
            "mut" => TokenKind::Mut,
            "const" => TokenKind::Const,
            "def" => TokenKind::Def,
            "class" => TokenKind::Class,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "elif" => TokenKind::Elif,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "enum" => TokenKind::Enum,
            _ => TokenKind::Identifier(lexeme.clone()),
        };
        
        self.create_token_with_pos(kind, lexeme, start_pos, self.current_pos)
    }
    
    fn scan_number(&mut self) -> Token {
        let start_pos = self.current_pos;
        let mut lexeme = String::new();
        
        while let Some(ch) = self.peek_char_option() {
            if ch.is_ascii_digit() {
                lexeme.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        
        let value = lexeme.parse::<i64>().unwrap_or(0);
        self.create_token_with_pos(TokenKind::IntegerLiteral(value), lexeme, start_pos, self.current_pos)
    }
    
    fn scan_string(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let mut lexeme = String::new();
        let mut value = String::new();
        
        lexeme.push('"');
        self.next_char(); // consume opening "
        
        while let Some(ch) = self.peek_char_option() {
            if ch == '"' {
                lexeme.push('"');
                self.next_char(); // consume closing "
                break;
            } else if ch == '\\' {
                // Simple escape handling
                lexeme.push('\\');
                self.next_char();
                if let Some(escaped) = self.peek_char_option() {
                    lexeme.push(escaped);
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '"' => '"',
                        '\\' => '\\',
                        _ => escaped,
                    });
                    self.next_char();
                }
            } else {
                lexeme.push(ch);
                value.push(ch);
                self.next_char();
            }
        }
        
        Ok(self.create_token_with_pos(TokenKind::StringLiteral(value), lexeme, start_pos, self.current_pos))
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char_option() {
            if ch.is_whitespace() && ch != '\n' {
                self.next_char();
            } else {
                break;
            }
        }
    }
    
    fn peek_char(&mut self) -> char {
        self.input.peek().copied().unwrap_or('\0')
    }
    
    fn peek_char_option(&mut self) -> Option<char> {
        self.input.peek().copied()
    }
    
    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.next()?;
        self.current_pos.advance(ch);
        Some(ch)
    }
    
    fn is_at_end(&mut self) -> bool {
        self.input.peek().is_none()
    }
    
    fn create_token(&self, kind: TokenKind, lexeme: &str) -> Token {
        Token::new(kind, Span::new(self.current_pos, self.current_pos), lexeme.to_string())
    }
    
    fn create_token_with_pos(&self, kind: TokenKind, lexeme: String, start: Position, end: Position) -> Token {
        Token::new(kind, Span::new(start, end), lexeme)
    }
}
