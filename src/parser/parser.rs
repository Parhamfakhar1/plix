use crate::lexer::{Lexer, Token, TokenKind};
use crate::utils::error::{CompilerError, CompilerResult};
use crate::utils::position::{Position, Span};
use super::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(source: &str) -> CompilerResult<Self> {
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize()?;
        
        Ok(Self {
            tokens,
            current: 0,
        })
    }
    
    pub fn parse(&mut self) -> CompilerResult<Program> {
        let start_pos = if self.tokens.is_empty() {
            Position::new()
        } else {
            self.peek().span.start
        };
        let mut statements = Vec::new();
        
        while !self.is_at_end() {
            self.skip_whitespace_and_comments();
            
            if self.is_at_end() {
                break;
            }
            
            if let Some(stmt) = self.parse_statement()? {
                // Only require semicolon for non-expression statements
                match &stmt {
                    Statement::Expression(_, _) => {
                        // Expressions don't need semicolons
                    },
                    _ => {
                        // Other statements require semicolons
                        self.expect(TokenKind::Semicolon, ";")?;
                    }
                }
                
                statements.push(stmt);
            }
        }
        
        let end_pos = if statements.is_empty() {
            start_pos
        } else {
            self.previous().span.end
        };
        Ok(Program {
            statements,
            span: Span::new(start_pos, end_pos),
        })
    }
    
    fn skip_whitespace_and_comments(&mut self) {
        while !self.is_at_end() {
            if self.check(TokenKind::Comment) || self.check(TokenKind::Newline) {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    fn parse_statement(&mut self) -> CompilerResult<Option<Statement>> {
        self.skip_whitespace_and_comments();
        
        if self.is_at_end() {
            return Ok(None);
        }
        
        if self.match_token(TokenKind::Enum) {
            self.parse_enum()
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::Def) {
            self.parse_function()
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::Const) {
            self.parse_constant()
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::Mut) {
            self.parse_variable(true)
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::If) {
            self.parse_if_statement()
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::Return) {
            self.parse_return()
                .map(|stmt| Some(stmt))
        } else if self.match_token(TokenKind::Class) {
            self.parse_class()
                .map(|stmt| Some(stmt))
        } else {
            // Parse expression statement - don't require semicolon for expressions
            self.parse_expression()
                .map(|expr| {
                    let span = expr.span();
                    Some(Statement::Expression(expr, span))
                })
        }
    }
    
    fn parse_function(&mut self) -> CompilerResult<Statement> {
        let name = self.expect_identifier("function name")?;
        
        self.expect(TokenKind::LParen, "(")?;
        
        let mut parameters = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                let param_name = self.expect_identifier("parameter name")?;
                let type_annotation = if self.match_token(TokenKind::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                
                let default_value = if self.match_token(TokenKind::Assign) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                
                parameters.push(Parameter {
                    name: param_name,
                    type_annotation,
                    default_value,
                });
                
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }
        
        self.expect(TokenKind::RParen, ")")?;
        
        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        // JavaScript style: require { } for function body
        self.expect(TokenKind::LBrace, "{")?;
        let body = self.parse_block_statements()?;
        self.expect(TokenKind::RBrace, "}")?;
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Function {
            name,
            parameters,
            return_type,
            body,
            async_flag: false,
            span,
        })
    }
    
    fn parse_constant(&mut self) -> CompilerResult<Statement> {
        let name = self.expect_identifier("constant name")?;
        
        let type_annotation = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        self.expect(TokenKind::Assign, "=")?;
        
        let value = self.parse_expression()?;
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Constant {
            name,
            type_annotation,
            value,
            span,
        })
    }
    
    fn parse_variable(&mut self, mutable: bool) -> CompilerResult<Statement> {
        let name = self.expect_identifier("variable name")?;
        
        let type_annotation = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        self.expect(TokenKind::Assign, "=")?;
        
        let value = self.parse_expression()?;
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Variable {
            mutable,
            name,
            type_annotation,
            value,
            span,
        })
    }
    
    fn parse_if_statement(&mut self) -> CompilerResult<Statement> {
        let condition = self.parse_expression()?;
        
        // JavaScript style: require { } for if body
        self.expect(TokenKind::LBrace, "{")?;
        let then_branch = self.parse_block_statements()?;
        self.expect(TokenKind::RBrace, "}")?;
        
        let mut elif_branches = Vec::new();
        let mut else_branch = None;
        
        while self.match_token(TokenKind::Elif) {
            let elif_condition = self.parse_expression()?;
            self.expect(TokenKind::LBrace, "{")?;
            let elif_body = self.parse_block_statements()?;
            self.expect(TokenKind::RBrace, "}")?;
            elif_branches.push((elif_condition, elif_body));
        }
        
        if self.match_token(TokenKind::Else) {
            self.expect(TokenKind::LBrace, "{")?;
            let else_body = self.parse_block_statements()?;
            self.expect(TokenKind::RBrace, "}")?;
            else_branch = Some(else_body);
        }
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
            span,
        })
    }
    
    fn parse_return(&mut self) -> CompilerResult<Statement> {
        let value = if !self.check(TokenKind::Semicolon) && !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Return(value, span))
    }
    
    fn parse_class(&mut self) -> CompilerResult<Statement> {
        let name = self.expect_identifier("class name")?;
        
        let base = if self.match_token(TokenKind::LParen) {
            let base_name = self.expect_identifier("base class name")?;
            self.expect(TokenKind::RParen, ")")?;
            Some(base_name)
        } else {
            None
        };
        
        self.expect(TokenKind::LBrace, "{")?;
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_whitespace_and_comments();
            
            if self.is_at_end() || self.check(TokenKind::RBrace) {
                break;
            }
            
            if self.match_token(TokenKind::Def) {
                let method = self.parse_function()?;
                methods.push(method);
                self.expect(TokenKind::Semicolon, ";")?;
            } else if let TokenKind::Identifier(_) = self.peek().kind {
                let field_name = self.expect_identifier("field name")?;
                self.expect(TokenKind::Colon, ":")?;
                let type_annotation = Some(self.parse_type()?);
                fields.push(ClassField {
                    name: field_name,
                    type_annotation,
                    visibility: Visibility::Public,
                });
                self.expect(TokenKind::Semicolon, ";")?;
            } else {
                return Err(self.error("Expected field or method definition in class"));
            }
        }
        
        self.expect(TokenKind::RBrace, "}")?;
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Class {
            name,
            base,
            fields,
            methods,
            span,
        })
    }
    
    fn parse_block_statements(&mut self) -> CompilerResult<Vec<Statement>> {
        let mut statements = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_whitespace_and_comments();
            
            if self.is_at_end() || self.check(TokenKind::RBrace) {
                break;
            }
            
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
                self.expect(TokenKind::Semicolon, ";")?;
            }
        }
        
        Ok(statements)
    }
    
    fn parse_expression(&mut self) -> CompilerResult<Expression> {
        self.parse_assignment()
    }
    
    fn parse_assignment(&mut self) -> CompilerResult<Expression> {
        let expr = self.parse_equality()?;
        
        if self.match_token(TokenKind::Assign) {
            let value = self.parse_assignment()?;
            let span = expr.span().merge(value.span());
            return Ok(Expression::Assignment {
                target: Box::new(expr),
                op: None,
                value: Box::new(value),
                span,
            });
        } else if self.match_compound_assignment() {
            let op = self.previous_op();
            self.expect(TokenKind::Assign, "=")?;
            let value = self.parse_assignment()?;
            let span = expr.span().merge(value.span());
            return Ok(Expression::Assignment {
                target: Box::new(expr),
                op: Some(op),
                value: Box::new(value),
                span,
            });
        }
        
        Ok(expr)
    }
    
    fn parse_equality(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_comparison()?;
        
        loop {
            self.skip_whitespace_and_comments();
            
            if self.check(TokenKind::Equal) {
                self.advance();
                let right = self.parse_comparison()?;
                expr = Expression::binary(expr, BinaryOp::Equal, right);
            } else if self.check(TokenKind::NotEqual) {
                self.advance();
                let right = self.parse_comparison()?;
                expr = Expression::binary(expr, BinaryOp::NotEqual, right);
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_term()?;
        
        loop {
            self.skip_whitespace_and_comments();
            
            if self.check(TokenKind::Less) {
                self.advance();
                let right = self.parse_term()?;
                expr = Expression::binary(expr, BinaryOp::Less, right);
            } else if self.check(TokenKind::LessEqual) {
                self.advance();
                let right = self.parse_term()?;
                expr = Expression::binary(expr, BinaryOp::LessEqual, right);
            } else if self.check(TokenKind::Greater) {
                self.advance();
                let right = self.parse_term()?;
                expr = Expression::binary(expr, BinaryOp::Greater, right);
            } else if self.check(TokenKind::GreaterEqual) {
                self.advance();
                let right = self.parse_term()?;
                expr = Expression::binary(expr, BinaryOp::GreaterEqual, right);
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_factor()?;
        
        loop {
            self.skip_whitespace_and_comments();
            
            if self.check(TokenKind::Plus) {
                self.advance();
                let right = self.parse_factor()?;
                expr = Expression::binary(expr, BinaryOp::Add, right);
            } else if self.check(TokenKind::Minus) {
                self.advance();
                let right = self.parse_factor()?;
                expr = Expression::binary(expr, BinaryOp::Subtract, right);
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_unary()?;
        
        loop {
            self.skip_whitespace_and_comments();
            
            if self.check(TokenKind::Star) {
                self.advance();
                let right = self.parse_unary()?;
                expr = Expression::binary(expr, BinaryOp::Multiply, right);
            } else if self.check(TokenKind::Slash) {
                self.advance();
                let right = self.parse_unary()?;
                expr = Expression::binary(expr, BinaryOp::Divide, right);
            } else if self.check(TokenKind::Percent) {
                self.advance();
                let right = self.parse_unary()?;
                expr = Expression::binary(expr, BinaryOp::Modulo, right);
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self) -> CompilerResult<Expression> {
        self.skip_whitespace_and_comments();
        
        if self.check(TokenKind::Plus) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expression::unary(UnaryOp::Plus, expr))
        } else if self.check(TokenKind::Minus) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expression::unary(UnaryOp::Minus, expr))
        } else if self.check(TokenKind::Bang) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expression::unary(UnaryOp::Not, expr))
        } else if self.check(TokenKind::Tilde) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expression::unary(UnaryOp::BitNot, expr))
        } else {
            self.parse_try()
        }
    }

    fn parse_try(&mut self) -> CompilerResult<Expression> {
        let expr = self.parse_primary()?;
        
        if self.match_token(TokenKind::Question) {
            let span = expr.span();
            Ok(Expression::Try {
                expr: Box::new(expr),
                span,
            })
        } else {
            Ok(expr)
        }
    }
    
    fn parse_primary(&mut self) -> CompilerResult<Expression> {
        self.skip_whitespace_and_comments();
        
        if self.match_token(TokenKind::IntegerLiteral(0)) {
            if let TokenKind::IntegerLiteral(value) = self.previous().kind {
                Ok(Expression::integer(value))
            } else {
                unreachable!()
            }
        } else if self.match_token(TokenKind::StringLiteral("".to_string())) {
            if let TokenKind::StringLiteral(value) = self.previous().kind.clone() {
                Ok(Expression::string(value))
            } else {
                unreachable!()
            }
        } else if self.match_token(TokenKind::True) {
            Ok(Expression::boolean(true))
        } else if self.match_token(TokenKind::False) {
            Ok(Expression::boolean(false))
        } else if self.match_token(TokenKind::Null) {
            let span = self.previous().span;
            Ok(Expression::Literal(Literal::Null, span))
        } else if self.match_token(TokenKind::Undefined) {
            let span = self.previous().span;
            Ok(Expression::Literal(Literal::Undefined, span))
        } else if let TokenKind::Identifier(_) = self.peek().kind {
            let token = self.advance();
            if let TokenKind::Identifier(name) = token.kind.clone() {
                // Check if this is a function call: name(...)
                if self.check(TokenKind::LParen) {
                    self.match_token(TokenKind::LParen); // consume (
                    let mut arguments = Vec::new();
                    if !self.check(TokenKind::RParen) {
                        loop {
                            arguments.push(self.parse_expression()?);
                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RParen, ")")?;
                    let func_expr = Expression::identifier(name);
                    let span = func_expr.span().merge(self.previous().span);
                    Ok(Expression::Call {
                        function: Box::new(func_expr),
                        arguments,
                        span,
                    })
                } else if self.check(TokenKind::Dot) {
                    // Handle member access: obj.property
                    self.advance(); // consume .
                    let member = self.expect_identifier("member name")?;
                    let span = self.previous().span;
                    let obj_expr = Expression::identifier(name);
                    Ok(Expression::Member {
                        expr: Box::new(obj_expr),
                        member,
                        span,
                    })
                } else if self.check(TokenKind::LBrace) {
                    // Handle object literal: { key: value, ... }
                    self.match_token(TokenKind::LBrace); // consume {
                    let mut fields = Vec::new();
                    if !self.check(TokenKind::RBrace) {
                        loop {
                            let key = self.expect_identifier("object key")?;
                            self.expect(TokenKind::Colon, ":")?;
                            let value = self.parse_expression()?;
                            fields.push((key, value));
                            
                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RBrace, "}")?;
                    let span = self.previous().span;
                    Ok(Expression::Literal(Literal::Object(fields), span))
                } else {
                    Ok(Expression::identifier(name))
                }
            } else {
                unreachable!()
            }
        } else if self.match_token(TokenKind::LParen) {
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RParen, ")")?;
            Ok(expr)
        } else if self.match_token(TokenKind::LBrace) {
            // Handle block expression: { statements }
            let mut statements = Vec::new();
            while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                if let Some(stmt) = self.parse_statement()? {
                    statements.push(stmt);
                    self.expect(TokenKind::Semicolon, ";")?;
                }
            }
            self.expect(TokenKind::RBrace, "}")?;
            let span = self.previous().span;
            Ok(Expression::Block {
                statements,
                span,
            })
        } else {
            Err(self.error("Expected expression"))
        }
    }
    
    fn parse_type(&mut self) -> CompilerResult<Type> {
        if self.match_token(TokenKind::Identifier("number".to_string())) {
            Ok(Type::Number)
        } else if self.match_token(TokenKind::Identifier("string".to_string())) {
            Ok(Type::String)
        } else if self.match_token(TokenKind::Identifier("boolean".to_string())) {
            Ok(Type::Boolean)
        } else if self.match_token(TokenKind::Identifier("void".to_string())) {
            Ok(Type::Void)
        } else if self.match_token(TokenKind::Identifier("null".to_string())) {
            Ok(Type::Null)
        } else if self.match_token(TokenKind::Identifier("undefined".to_string())) {
            Ok(Type::Undefined)
        } else if self.match_token(TokenKind::Identifier("Result".to_string())) {
            self.expect(TokenKind::Less, "<")?;
            let ok_type = self.parse_type()?;
            self.expect(TokenKind::Comma, ",")?;
            let err_type = self.parse_type()?;
            self.expect(TokenKind::Greater, ">")?;
            Ok(Type::Result(Box::new(ok_type), Box::new(err_type)))
        } else if self.match_token(TokenKind::Identifier("Array".to_string())) {
            self.expect(TokenKind::Less, "<")?;
            let inner_type = self.parse_type()?;
            self.expect(TokenKind::Greater, ">")?;
            Ok(Type::Array(Box::new(inner_type)))
        } else if self.match_token(TokenKind::Identifier("Optional".to_string())) {
            self.expect(TokenKind::Less, "<")?;
            let inner_type = self.parse_type()?;
            self.expect(TokenKind::Greater, ">")?;
            Ok(Type::Optional(Box::new(inner_type)))
        } else {
            let name = self.expect_identifier("type name")?;
            Ok(Type::Custom(name))
        }
    }
    
    fn parse_enum(&mut self) -> CompilerResult<Statement> {
        let name = self.expect_identifier("enum name")?;
        
        let mut generics = Vec::new();
        if self.match_token(TokenKind::Less) {
            loop {
                let generic_name = self.expect_identifier("generic parameter")?;
                generics.push(generic_name);
                
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::Greater, ">")?;
        }
        
        self.expect(TokenKind::LBrace, "{")?;
        
        let mut variants = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let variant_name = self.expect_identifier("variant name")?;
            
            let mut fields = Vec::new();
            if self.match_token(TokenKind::LParen) {
                if !self.check(TokenKind::RParen) {
                    loop {
                        let field_type = self.parse_type()?;
                        fields.push(field_type);
                        
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RParen, ")")?;
            }
            
            variants.push(EnumVariant {
                name: variant_name,
                fields,
            });
            
            if self.check(TokenKind::RBrace) {
                break;
            }
            
            self.expect(TokenKind::Comma, ",")?;
        }
        
        self.expect(TokenKind::RBrace, "}")?;
        
        let span = Span::new(self.tokens[0].span.start, self.previous().span.end);
        Ok(Statement::Enum {
            name,
            generics,
            variants,
            span,
        })
    }
    
    // --- Helper methods ---
    
    fn match_compound_assignment(&mut self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::PlusAssign |
            TokenKind::MinusAssign |
            TokenKind::StarAssign |
            TokenKind::SlashAssign |
            TokenKind::PercentAssign
        )
    }
    
    fn previous_op(&self) -> BinaryOp {
        match self.previous().kind {
            TokenKind::PlusAssign => BinaryOp::Add,
            TokenKind::MinusAssign => BinaryOp::Subtract,
            TokenKind::StarAssign => BinaryOp::Multiply,
            TokenKind::SlashAssign => BinaryOp::Divide,
            TokenKind::PercentAssign => BinaryOp::Modulo,
            _ => unreachable!(),
        }
    }
    
    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        
        let current_kind = &self.peek().kind;
        
        // Handle literal types specially - only check the variant, not the value
        match (current_kind, &kind) {
            (TokenKind::IntegerLiteral(_), TokenKind::IntegerLiteral(_)) => true,
            (TokenKind::StringLiteral(_), TokenKind::StringLiteral(_)) => true,
            (TokenKind::Identifier(a), TokenKind::Identifier(b)) if a == b => true,
            _ => current_kind == &kind,
        }
    }
    
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
    
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().kind == TokenKind::EOF
    }
    
    fn expect(&mut self, kind: TokenKind, what: &str) -> CompilerResult<()> {
        if self.match_token(kind) {
            Ok(())
        } else {
            let token = self.peek();
            Err(CompilerError::Parser {
                span: token.span,
                message: format!("Expected {}, found {}", what, token.kind),
            })
        }
    }
    
    fn expect_identifier(&mut self, what: &str) -> CompilerResult<String> {
        if let TokenKind::Identifier(name) = self.peek().kind.clone() {
            self.advance();
            Ok(name)
        } else {
            let token = self.peek();
            Err(CompilerError::Parser {
                span: token.span,
                message: format!("Expected {}, found {}", what, token.kind),
            })
        }
    }
    
    fn error(&self, message: &str) -> CompilerError {
        let token = if self.is_at_end() {
            self.previous()
        } else {
            self.peek()
        };
        CompilerError::Parser {
            span: token.span,
            message: message.to_string(),
        }
    }
}
