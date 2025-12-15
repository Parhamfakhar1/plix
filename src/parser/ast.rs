#![allow(dead_code)]

use std::fmt;
use crate::utils::position::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Null,
    Undefined,
    Array(Box<Type>),
    Object(Vec<(String, Type)>),
    Tuple(Vec<Type>),
    Optional(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Custom(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::Void => write!(f, "void"),
            Type::Null => write!(f, "null"),
            Type::Undefined => write!(f, "undefined"),
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::Object(fields) => {
                write!(f, "{{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", name, ty)?;
                }
                write!(f, "}}")
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            Type::Optional(inner) => write!(f, "{}?", inner),
            Type::Result(ok, err) => write!(f, "Result<{}, {}>", ok, err),
            Type::Custom(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    Array(Vec<Expression>),
    Object(Vec<(String, Expression)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Range,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Modulo => write!(f, "%"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::ShiftLeft => write!(f, "<<"),
            BinaryOp::ShiftRight => write!(f, ">>"),
            BinaryOp::Range => write!(f, ".."),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    BitNot,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
        }
    }
}



#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String, Span),
    Literal(Literal, Span),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
        span: Span,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
    Index {
        expr: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },
    Member {
        expr: Box<Expression>,
        member: String,
        span: Span,
    },
    Assignment {
        target: Box<Expression>,
        op: Option<BinaryOp>,
        value: Box<Expression>,
        span: Span,
    },
    Lambda {
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Box<Statement>,
        span: Span,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        span: Span,
    },
    Match {
        expr: Box<Expression>,
        arms: Vec<MatchArm>,
        span: Span,
    },
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Identifier(_, span) => *span,
            Expression::Literal(_, span) => *span,
            Expression::Binary { span, .. } => *span,
            Expression::Unary { span, .. } => *span,
            Expression::Call { span, .. } => *span,
            Expression::Index { span, .. } => *span,
            Expression::Member { span, .. } => *span,
            Expression::Assignment { span, .. } => *span,
            Expression::Lambda { span, .. } => *span,
            Expression::If { span, .. } => *span,
            Expression::Match { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Identifier(String),
    Tuple(Vec<Pattern>),
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression, Span),
    Variable {
        mutable: bool,
        name: String,
        type_annotation: Option<Type>,
        value: Expression,
        span: Span,
    },
    Constant {
        name: String,
        type_annotation: Option<Type>,
        value: Expression,
        span: Span,
    },
    Return(Option<Expression>, Span),
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        elif_branches: Vec<(Expression, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
        span: Span,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
        span: Span,
    },
    For {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
        span: Span,
    },
    Match {
        expr: Expression,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Block(Vec<Statement>, Span),
    Function {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>,
        async_flag: bool,
        span: Span,
    },
    Class {
        name: String,
        base: Option<String>,
        fields: Vec<ClassField>,
        methods: Vec<Statement>,
        span: Span,
    },
    Import {
        module: String,
        alias: Option<String>,
        items: Option<Vec<ImportItem>>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl Expression {
    pub fn identifier(name: String) -> Self {
        Expression::Identifier(name, Span::default())
    }
    
    pub fn integer(value: i64) -> Self {
        Expression::Literal(Literal::Integer(value), Span::default())
    }
    
    pub fn float(value: f64) -> Self {
        Expression::Literal(Literal::Float(value), Span::default())
    }
    
    pub fn string(value: String) -> Self {
        Expression::Literal(Literal::String(value), Span::default())
    }
    
    pub fn boolean(value: bool) -> Self {
        Expression::Literal(Literal::Boolean(value), Span::default())
    }
    
    pub fn binary(left: Expression, op: BinaryOp, right: Expression) -> Self {
        let span = left.span().merge(right.span());
        Expression::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        }
    }
    
    pub fn unary(op: UnaryOp, expr: Expression) -> Self {
        let span = expr.span();
        Expression::Unary {
            op,
            expr: Box::new(expr),
            span,
        }
    }
    
    pub fn call(function: Expression, arguments: Vec<Expression>) -> Self {
        let span = function.span();
        Expression::Call {
            function: Box::new(function),
            arguments,
            span,
        }
    }
}

impl Statement {
    pub fn expr(expr: Expression) -> Self {
        let span = expr.span();
        Statement::Expression(expr, span)
    }
    
    pub fn variable(mutable: bool, name: String, value: Expression) -> Self {
        let span = value.span();
        Statement::Variable {
            mutable,
            name,
            type_annotation: None,
            value,
            span,
        }
    }
    
    pub fn constant(name: String, value: Expression) -> Self {
        let span = value.span();
        Statement::Constant {
            name,
            type_annotation: None,
            value,
            span,
        }
    }
    
    pub fn function(name: String, parameters: Vec<Parameter>, body: Vec<Statement>) -> Self {
        let span = Span::default();
        Statement::Function {
            name,
            parameters,
            return_type: None,
            body,
            async_flag: false,
            span,
        }
    }
    
    pub fn span(&self) -> Span {
        match self {
            Statement::Expression(_, span) => *span,
            Statement::Variable { span, .. } => *span,
            Statement::Constant { span, .. } => *span,
            Statement::Return(_, span) => *span,
            Statement::If { span, .. } => *span,
            Statement::While { span, .. } => *span,
            Statement::For { span, .. } => *span,
            Statement::Match { span, .. } => *span,
            Statement::Block(_, span) => *span,
            Statement::Function { span, .. } => *span,
            Statement::Class { span, .. } => *span,
            Statement::Import { span, .. } => *span,
        }
    }
}

impl Pattern {
    pub fn span(&self) -> Span {
        // For now, return a default span since patterns don't have span info
        Span::default()
    }
}
