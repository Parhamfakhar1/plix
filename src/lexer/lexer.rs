use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::token::{Token, TokenKind};
use crate::utils::error::{LexerError, CompilerResult};
use crate::utils::position::{Position, Span};

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_pos: Position,
    pending_tokens: Vec<Token>,
    indent_stack: Vec<usize>,
    is_start_of_line: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            current_pos: Position::new(),
            pending_tokens: Vec::new(),
            indent_stack: vec![0],
            is_start_of_line: true,
        }
    }
    
    pub fn tokenize(mut self) -> CompilerResult<Vec<Token>> {
        let mut tokens = Vec::new();
        
        while let Some(token) = self.next_token()? {
            tokens.push(token);
        }
        
        while self.indent_stack.len() > 1 {
            tokens.push(self.create_token(TokenKind::Dedent, "".to_string()));
            self.indent_stack.pop();
        }
        
        tokens.push(Token::eof(self.current_pos));
        Ok(tokens)
    }
    
    fn next_token(&mut self) -> CompilerResult<Option<Token>> {
        if let Some(token) = self.pending_tokens.pop() {
            return Ok(Some(token));
        }
        
        self.skip_whitespace_except_newline();
        
        let start_pos = self.current_pos;
        
        let next_char = match self.input.peek() {
            Some(&ch) => ch,
            None => return Ok(None),
        };
        
        if self.is_start_of_line && next_char != '\n' && next_char != '\r' {
            self.handle_indentation()?;
            if let Some(token) = self.pending_tokens.pop() {
                return Ok(Some(token));
            }
        }
        
        let token = match next_char {
            '#' => self.scan_comment(),
            '"' => self.scan_string(),
            '\'' => self.scan_char(),
            
            '0'..='9' => self.scan_number(),
            
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.scan_identifier(),
            
            '+' => self.scan_plus(),
            '-' => self.scan_minus(),
            '*' => self.scan_star(),
            '/' => self.scan_slash(),
            '%' => self.scan_percent(),
            '^' => self.scan_caret(),
            '!' => self.scan_bang(),
            '=' => self.scan_equal(),
            '<' => self.scan_less(),
            '>' => self.scan_greater(),
            '&' => self.scan_amp(),
            '|' => self.scan_pipe(),
            '~' => self.scan_tilde(),
            '.' => self.scan_dot(),
            ',' => self.consume_char(TokenKind::Comma),
            ':' => self.scan_colon(),
            ';' => self.consume_char(TokenKind::Semicolon),
            '?' => self.consume_char(TokenKind::Question),
            '(' => self.consume_char(TokenKind::LParen),
            ')' => self.consume_char(TokenKind::RParen),
            '[' => self.consume_char(TokenKind::LBracket),
            ']' => self.consume_char(TokenKind::RBracket),
            '{' => self.consume_char(TokenKind::LBrace),
            '}' => self.consume_char(TokenKind::RBrace),
            
            '\n' => {
                let token = self.consume_char(TokenKind::Newline);
                self.is_start_of_line = true;
                token
            }
            '\r' => {
                self.next_char();
                if self.input.peek() == Some(&'\n') {
                    self.next_char();
                }
                let token = self.create_token_with_pos(TokenKind::Newline, "\n".to_string(), start_pos, self.current_pos);
                self.is_start_of_line = true;
                Ok(token)
            }
            
            ch => {
                self.next_char();
                return Err(LexerError::UnexpectedCharacter {
                    character: ch,
                    position: start_pos,
                }.into());
            }
        }?;
        
        self.is_start_of_line = false;
        Ok(Some(token))
    }
    
    fn handle_indentation(&mut self) -> CompilerResult<()> {
        let start_pos = self.current_pos;
        let mut indent_level = 0;
        
        while let Some(&ch) = self.input.peek() {
            match ch {
                ' ' => {
                    indent_level += 1;
                    self.next_char();
                }
                '\t' => {
                    indent_level += 4;
                    self.next_char();
                }
                _ => break,
            }
        }
        
        if let Some(&ch) = self.input.peek() {
            if ch == '#' {
                self.scan_comment()?;
                self.is_start_of_line = true;
                return Ok(());
            }
            
            if ch == '\n' || ch == '\r' {
                self.is_start_of_line = true;
                return Ok(());
            }
        }
        
        let current_indent = *self.indent_stack.last().unwrap();
        
        if indent_level > current_indent {
            self.indent_stack.push(indent_level);
            self.pending_tokens.push(self.create_token_with_pos(
                TokenKind::Indent,
                "".to_string(),
                start_pos,
                self.current_pos,
            ));
        } else if indent_level < current_indent {
            while let Some(&stack_indent) = self.indent_stack.last() {
                if stack_indent > indent_level {
                    self.indent_stack.pop();
                    self.pending_tokens.push(self.create_token_with_pos(
                        TokenKind::Dedent,
                        "".to_string(),
                        start_pos,
                        self.current_pos,
                    ));
                } else {
                    break;
                }
            }
            
            if *self.indent_stack.last().unwrap() != indent_level {
                return Err(LexerError::UnexpectedCharacter {
                    character: ' ',
                    position: start_pos,
                }.into());
            }
        }
        
        Ok(())
    }
    
    fn scan_comment(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        
        if self.input.peek() == Some(&'*') {
            return self.scan_block_comment(start_pos);
        }
        
        while let Some(&ch) = self.input.peek() {
            if ch == '\n' || ch == '\r' {
                break;
            }
            self.next_char();
        }
        
        self.next_token()
            .transpose()
            .unwrap_or(Ok(Token::eof(self.current_pos)))
    }
    
    fn scan_block_comment(&mut self, start_pos: Position) -> CompilerResult<Token> {
        let mut depth = 1;
        
        while depth > 0 {
            match self.input.next() {
                Some('*') => {
                    if self.input.peek() == Some(&'#') {
                        depth -= 1;
                        self.next_char(); // consume the '#'
                    }
                }
                Some('#') => {
                    if self.input.peek() == Some(&'*') {
                        depth += 1;
                        self.next_char(); // consume the '*'
                    }
                }
                Some(_) => {
                    // Continue scanning
                }
                None => {
                    return Err(LexerError::UnterminatedComment {
                        position: start_pos,
                    }.into());
                }
            }
        }
        
        self.next_token()
            .transpose()
            .unwrap_or(Ok(Token::eof(self.current_pos)))
    }
    
    fn scan_string(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let mut value = String::new();
        let mut lexeme = String::new();
        
        lexeme.push('"');
        
        let mut terminated = false;
        
        while let Some(&ch) = self.input.peek() {
            match ch {
                '"' => {
                    lexeme.push('"');
                    self.next_char();
                    terminated = true;
                    break;
                }
                '\\' => {
                    lexeme.push('\\');
                    if let Some(escaped_ch) = self.input.next() {
                        lexeme.push(escaped_ch);
                        value.push(self.escape_char(escaped_ch, self.current_pos)?);
                    }
                }
                '$' => {
                    if self.input.peek().is_some_and(|&c| c == '{') {
                        lexeme.push('$');
                        if let Some('{') = self.input.next() {
                            lexeme.push('{');
                        }
                        value.push_str("${");
                    } else {
                        value.push('$');
                        lexeme.push('$');
                        self.next_char();
                    }
                }
                '\n' | '\r' => {
                    break;
                }
                _ => {
                    value.push(ch);
                    lexeme.push(ch);
                    self.next_char();
                }
            }
        }
        
        if !terminated {
            return Err(LexerError::UnterminatedString {
                position: start_pos,
            }.into());
        }
        
        Ok(self.create_token_with_pos(
            TokenKind::StringLiteral(value),
            lexeme,
            start_pos,
            self.current_pos,
        ))
    }
    
    fn scan_char(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let mut value = String::new();
        let mut lexeme = String::new();
        
        lexeme.push('\'');
        
        if let Some(&ch) = self.input.peek() {
            if ch == '\\' {
                lexeme.push('\\');
                if let Some(escaped_ch) = self.input.next() {
                    lexeme.push(escaped_ch);
                    value.push(self.escape_char(escaped_ch, self.current_pos)?);
                }
            } else {
                value.push(ch);
                lexeme.push(ch);
                self.next_char();
            }
        } else {
            return Err(LexerError::UnterminatedString {
                position: start_pos,
            }.into());
        }
        
        if self.input.peek() != Some(&'\'') {
            return Err(LexerError::UnterminatedString {
                position: start_pos,
            }.into());
        }
        self.next_char();
        lexeme.push('\'');
        
        Ok(self.create_token_with_pos(
            TokenKind::StringLiteral(value),
            lexeme,
            start_pos,
            self.current_pos,
        ))
    }
    
    fn escape_char(&self, ch: char, position: Position) -> CompilerResult<char> {
        match ch {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            '\'' => Ok('\''),
            '0' => Ok('\0'),
            _ => Err(LexerError::UnknownEscapeSequence {
                character: ch,
                position,
            }.into()),
        }
    }
    
    fn scan_number(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let mut lexeme = String::new();
        let mut is_float = false;
        
        while let Some(&ch) = self.input.peek() {
            match ch {
                '0'..='9' => {
                    lexeme.push(ch);
                    self.next_char();
                }
                '.' => {
                    if is_float {
                        break;
                    }
                    is_float = true;
                    lexeme.push(ch);
                    self.next_char();
                }
                'e' | 'E' => {
                    is_float = true;
                    lexeme.push(ch);
                    self.next_char();
                    
                    if let Some(&sign) = self.input.peek() {
                        if sign == '+' || sign == '-' {
                            lexeme.push(sign);
                            self.next_char();
                        }
                    }
                }
                '_' => {
                    self.next_char();
                }
                _ => break,
            }
        }
        
        if is_float {
            let value = lexeme
                .parse::<f64>()
                .map_err(|_| LexerError::InvalidNumberFormat {
                    text: lexeme.clone(),
                    position: start_pos,
                })?;
            Ok(self.create_token_with_pos(
                TokenKind::NumberLiteral(value),
                lexeme,
                start_pos,
                self.current_pos,
            ))
        } else {
            let value = lexeme
                .parse::<i64>()
                .map_err(|_| LexerError::InvalidNumberFormat {
                    text: lexeme.clone(),
                    position: start_pos,
                })?;
            Ok(self.create_token_with_pos(
                TokenKind::IntegerLiteral(value),
                lexeme,
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_identifier(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let mut lexeme = String::new();
        
        if let Some(&ch) = self.input.peek() {
            if ch.is_alphabetic() || ch == '_' {
                lexeme.push(ch);
                self.next_char();
            }
        }
        
        while let Some(&ch) = self.input.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                lexeme.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        
        let kind = match lexeme.as_str() {
            "def" => TokenKind::Def,
            "const" => TokenKind::Const,
            "mut" => TokenKind::Mut,
            "class" => TokenKind::Class,
            "enum" => TokenKind::Enum,
            "data" => TokenKind::Data,
            "if" => TokenKind::If,
            "elif" => TokenKind::Elif,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "match" => TokenKind::Match,
            "case" => TokenKind::Case,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "import" => TokenKind::Import,
            "from" => TokenKind::From,
            "return" => TokenKind::Return,
            "true" => TokenKind::BooleanLiteral(true),
            "false" => TokenKind::BooleanLiteral(false),
            "null" => TokenKind::Null,
            "undefined" => TokenKind::Undefined,
            _ => TokenKind::Identifier(lexeme.clone()),
        };
        
        Ok(self.create_token_with_pos(
            kind,
            lexeme,
            start_pos,
            self.current_pos,
        ))
    }
    
    fn scan_plus(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'=') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::PlusAssign,
                "+=".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Plus,
                "+".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_minus(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        match self.input.peek() {
            Some(&'=') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::MinusAssign,
                    "-=".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            Some(&'>') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::Arrow,
                    "->".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            _ => Ok(self.create_token_with_pos(
                TokenKind::Minus,
                "-".to_string(),
                start_pos,
                self.current_pos,
            )),
        }
    }
    
    fn scan_star(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'=') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::StarAssign,
                "*=".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Star,
                "*".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_slash(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'=') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::SlashAssign,
                "/=".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Slash,
                "/".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_percent(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'=') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::PercentAssign,
                "%=".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Percent,
                "%".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_caret(&mut self) -> CompilerResult<Token> {
        self.consume_char(TokenKind::Caret)
    }
    
    fn scan_bang(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'=') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::NotEqual,
                "!=".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Bang,
                "!".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_equal(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        match self.input.peek() {
            Some(&'=') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::Equal,
                    "==".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            Some(&'>') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::Arrow,
                    "=>".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            _ => Ok(self.create_token_with_pos(
                TokenKind::Assign,
                "=".to_string(),
                start_pos,
                self.current_pos,
            )),
        }
    }
    
    fn scan_less(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        match self.input.peek() {
            Some(&'=') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::LessEqual,
                    "<=".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            Some(&'<') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::Shl,
                    "<<".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            _ => Ok(self.create_token_with_pos(
                TokenKind::Less,
                "<".to_string(),
                start_pos,
                self.current_pos,
            )),
        }
    }
    
    fn scan_greater(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        match self.input.peek() {
            Some(&'=') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::GreaterEqual,
                    ">=".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            Some(&'>') => {
                self.next_char();
                Ok(self.create_token_with_pos(
                    TokenKind::Shr,
                    ">>".to_string(),
                    start_pos,
                    self.current_pos,
                ))
            }
            _ => Ok(self.create_token_with_pos(
                TokenKind::Greater,
                ">".to_string(),
                start_pos,
                self.current_pos,
            )),
        }
    }
    
    fn scan_amp(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'&') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::And,
                "&&".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Amp,
                "&".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_pipe(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&'|') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::Or,
                "||".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Pipe,
                "|".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn scan_tilde(&mut self) -> CompilerResult<Token> {
        self.consume_char(TokenKind::Tilde)
    }
    
    fn scan_dot(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        match self.input.peek() {
            Some(&'.') => {
                self.next_char();
                if self.input.peek() == Some(&'.') {
                    self.next_char();
                    Ok(self.create_token_with_pos(
                        TokenKind::Ellipsis,
                        "...".to_string(),
                        start_pos,
                        self.current_pos,
                    ))
                } else {
                    Ok(self.create_token_with_pos(
                        TokenKind::Range,
                        "..".to_string(),
                        start_pos,
                        self.current_pos,
                    ))
                }
            }
            _ => Ok(self.create_token_with_pos(
                TokenKind::Dot,
                ".".to_string(),
                start_pos,
                self.current_pos,
            )),
        }
    }
    
    fn scan_colon(&mut self) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        
        if self.input.peek() == Some(&':') {
            self.next_char();
            Ok(self.create_token_with_pos(
                TokenKind::DoubleColon,
                "::".to_string(),
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(self.create_token_with_pos(
                TokenKind::Colon,
                ":".to_string(),
                start_pos,
                self.current_pos,
            ))
        }
    }
    
    fn consume_char(&mut self, kind: TokenKind) -> CompilerResult<Token> {
        let start_pos = self.current_pos;
        let ch = self.input.next().unwrap();
        let lexeme = ch.to_string();
        let token = self.create_token_with_pos(kind, lexeme, start_pos, self.current_pos);
        self.current_pos.advance(ch);
        Ok(token)
    }
    
    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.next()?;
        self.current_pos.advance(ch);
        Some(ch)
    }
    
    fn skip_whitespace_except_newline(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch == ' ' || ch == '\t' {
                self.next_char();
            } else {
                break;
            }
        }
    }
    
    fn create_token(&self, kind: TokenKind, lexeme: String) -> Token {
        Token::new(kind, Span::new(self.current_pos, self.current_pos), lexeme)
    }
    
    fn create_token_with_pos(&self, kind: TokenKind, lexeme: String, start: Position, end: Position) -> Token {
        Token::new(kind, Span::new(start, end), lexeme)
    }
}
