use crate::lexer::{Lexer, Token, TokenKind};
use crate::utils::error::{CompilerError, CompilerResult};
use crate::utils::position::Span;
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
        let start_pos = self.peek().span.start;
        let mut statements = Vec::new();
        
        while !self.is_at_end() {
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
            }
        }
        
        let end_pos = self.previous().span.end;
        Ok(Program {
            statements,
            span: Span::new(start_pos, end_pos),
        })
    }
    
    fn parse_statement(&mut self) -> CompilerResult<Option<Statement>> {
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
        } else if self.check(TokenKind::Identifier("".to_string())) {
            self.parse_possible_assignment()
        } else {
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
        
        self.expect(TokenKind::Colon, ":")?;
        
        let body = self.parse_block()?;
        
        Ok(Statement::Function {
            name,
            parameters,
            return_type,
            body,
            async_flag: false,
            span: Span::default(),
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
        
        Ok(Statement::Constant {
            name,
            type_annotation,
            value,
            span: Span::default(),
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
        
        Ok(Statement::Variable {
            mutable,
            name,
            type_annotation,
            value,
            span: Span::default(),
        })
    }
    
    fn parse_if_statement(&mut self) -> CompilerResult<Statement> {
        let condition = self.parse_expression()?;
        self.expect(TokenKind::Colon, ":")?;
        
        let then_branch = self.parse_block()?;
        
        let mut elif_branches = Vec::new();
        let mut else_branch = None;
        
        while self.match_token(TokenKind::Elif) {
            let elif_condition = self.parse_expression()?;
            self.expect(TokenKind::Colon, ":")?;
            let elif_body = self.parse_block()?;
            elif_branches.push((elif_condition, elif_body));
        }
        
        if self.match_token(TokenKind::Else) {
            self.expect(TokenKind::Colon, ":")?;
            else_branch = Some(self.parse_block()?);
        }
        
        Ok(Statement::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
            span: Span::default(),
        })
    }
    
    fn parse_return(&mut self) -> CompilerResult<Statement> {
        let value = if !self.check(TokenKind::Newline) && !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Statement::Return(value, Span::default()))
    }
    
    fn parse_possible_assignment(&mut self) -> CompilerResult<Option<Statement>> {
        let name = self.expect_identifier("identifier")?;
        
        if self.match_token(TokenKind::Assign) {
            let value = self.parse_expression()?;
            Ok(Some(Statement::Variable {
                mutable: false,
                name,
                type_annotation: None,
                value,
                span: Span::default(),
            }))
        } else {
            let expr = self.parse_expression()?;
            Ok(Some(Statement::Expression(expr, Span::default())))
        }
    }
    
    fn parse_block(&mut self) -> CompilerResult<Vec<Statement>> {
        let mut statements = Vec::new();
        
        self.expect(TokenKind::Indent, "indent")?;
        
        while !self.check(TokenKind::Dedent) && !self.is_at_end() {
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
            }
            
            while self.match_token(TokenKind::Newline) {}
        }
        
        self.expect(TokenKind::Dedent, "dedent")?;
        
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
        
        while let Some(op) = self.match_equality_op() {
            let right = self.parse_comparison()?;
            expr = Expression::binary(expr, op, right);
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_term()?;
        
        while let Some(op) = self.match_comparison_op() {
            let right = self.parse_term()?;
            expr = Expression::binary(expr, op, right);
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_factor()?;
        
        while let Some(op) = self.match_term_op() {
            let right = self.parse_factor()?;
            expr = Expression::binary(expr, op, right);
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_unary()?;
        
        while let Some(op) = self.match_factor_op() {
            let right = self.parse_unary()?;
            expr = Expression::binary(expr, op, right);
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self) -> CompilerResult<Expression> {
        if let Some(op) = self.match_unary_op() {
            let expr = self.parse_unary()?;
            return Ok(Expression::unary(op, expr));
        }
        
        self.parse_try()
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
        if self.match_token(TokenKind::IntegerLiteral(0)) {
            if let TokenKind::IntegerLiteral(value) = self.previous().kind {
                return Ok(Expression::integer(value));
            }
        } else if self.match_token(TokenKind::NumberLiteral(0.0)) {
            if let TokenKind::NumberLiteral(value) = self.previous().kind {
                return Ok(Expression::float(value));
            }
        } else if self.match_token(TokenKind::StringLiteral("".to_string())) {
            if let TokenKind::StringLiteral(value) = self.previous().kind.clone() {
                return Ok(Expression::string(value));
            }
        } else if self.match_token(TokenKind::BooleanLiteral(false)) {
            if let TokenKind::BooleanLiteral(value) = self.previous().kind {
                return Ok(Expression::boolean(value));
            }
        } else if self.match_token(TokenKind::True) {
            return Ok(Expression::boolean(true));
        } else if self.match_token(TokenKind::False) {
            return Ok(Expression::boolean(false));
        } else if self.match_token(TokenKind::Null) {
            let span = self.previous().span;
            return Ok(Expression::Literal(Literal::Null, span));
        } else if self.match_token(TokenKind::Undefined) {
            let span = self.previous().span;
            return Ok(Expression::Literal(Literal::Undefined, span));
        } else if self.match_token(TokenKind::Identifier("".to_string())) {
            if let TokenKind::Identifier(name) = self.previous().kind.clone() {
                // Check if this is a variant call
                if self.match_token(TokenKind::LParen) {
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
                    return Ok(Expression::VariantCall {
                        enum_name: "".to_string(), // We'll need to resolve this later
                        variant_name: name,
                        arguments,
                        span: Span::default(),
                    });
                } else {
                    return Ok(Expression::identifier(name));
                }
            }
        } else if self.match_token(TokenKind::LParen) {
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RParen, ")")?;
            return Ok(expr);
        }
        
        Err(self.error("Expected expression"))
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
    
    fn match_equality_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(TokenKind::Equal) {
            Some(BinaryOp::Equal)
        } else if self.match_token(TokenKind::NotEqual) {
            Some(BinaryOp::NotEqual)
        } else {
            None
        }
    }
    
    fn match_comparison_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(TokenKind::Less) {
            Some(BinaryOp::Less)
        } else if self.match_token(TokenKind::LessEqual) {
            Some(BinaryOp::LessEqual)
        } else if self.match_token(TokenKind::Greater) {
            Some(BinaryOp::Greater)
        } else if self.match_token(TokenKind::GreaterEqual) {
            Some(BinaryOp::GreaterEqual)
        } else {
            None
        }
    }
    
    fn match_term_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(TokenKind::Plus) {
            Some(BinaryOp::Add)
        } else if self.match_token(TokenKind::Minus) {
            Some(BinaryOp::Subtract)
        } else {
            None
        }
    }
    
    fn match_factor_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(TokenKind::Star) {
            Some(BinaryOp::Multiply)
        } else if self.match_token(TokenKind::Slash) {
            Some(BinaryOp::Divide)
        } else if self.match_token(TokenKind::Percent) {
            Some(BinaryOp::Modulo)
        } else {
            None
        }
    }
    
    fn match_unary_op(&mut self) -> Option<UnaryOp> {
        if self.match_token(TokenKind::Plus) {
            Some(UnaryOp::Plus)
        } else if self.match_token(TokenKind::Minus) {
            Some(UnaryOp::Minus)
        } else if self.match_token(TokenKind::Bang) {
            Some(UnaryOp::Not)
        } else if self.match_token(TokenKind::Tilde) {
            Some(UnaryOp::BitNot)
        } else {
            None
        }
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
            false
        } else {
            self.peek().kind == kind
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
        self.peek().kind == TokenKind::EOF
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
        let token = self.peek();
        CompilerError::Parser {
            span: token.span,
            message: message.to_string(),
        }
    }
}
