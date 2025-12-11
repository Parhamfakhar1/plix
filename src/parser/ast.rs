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
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String),
    Literal(Literal),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Index {
        expr: Box<Expression>,
        index: Box<Expression>,
    },
    Member {
        expr: Box<Expression>,
        member: String,
    },
    Assignment {
        target: Box<Expression>,
        op: Option<BinaryOp>,
        value: Box<Expression>,
    },
    Lambda {
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Box<Statement>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Match {
        expr: Box<Expression>,
        arms: Vec<MatchArm>,
    },
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
    Expression(Expression),
    Variable {
        mutable: bool,
        name: String,
        type_annotation: Option<Type>,
        value: Expression,
    },
    Constant {
        name: String,
        type_annotation: Option<Type>,
        value: Expression,
    },
    Return(Option<Expression>),
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        elif_branches: Vec<(Expression, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    For {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    Match {
        expr: Expression,
        arms: Vec<MatchArm>,
    },
    Block(Vec<Statement>),
    Function {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>,
        async_flag: bool,
    },
    Class {
        name: String,
        base: Option<String>,
        fields: Vec<ClassField>,
        methods: Vec<Statement>,
    },
    Import {
        module: String,
        alias: Option<String>,
        items: Option<Vec<ImportItem>>,
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
        Expression::Identifier(name)
    }
    
    pub fn integer(value: i64) -> Self {
        Expression::Literal(Literal::Integer(value))
    }
    
    pub fn float(value: f64) -> Self {
        Expression::Literal(Literal::Float(value))
    }
    
    pub fn string(value: String) -> Self {
        Expression::Literal(Literal::String(value))
    }
    
    pub fn boolean(value: bool) -> Self {
        Expression::Literal(Literal::Boolean(value))
    }
    
    pub fn binary(left: Expression, op: BinaryOp, right: Expression) -> Self {
        Expression::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
    
    pub fn unary(op: UnaryOp, expr: Expression) -> Self {
        Expression::Unary {
            op,
            expr: Box::new(expr),
        }
    }
    
    pub fn call(function: Expression, arguments: Vec<Expression>) -> Self {
        Expression::Call {
            function: Box::new(function),
            arguments,
        }
    }
}

impl Statement {
    pub fn expr(expr: Expression) -> Self {
        Statement::Expression(expr)
    }
    
    pub fn variable(mutable: bool, name: String, value: Expression) -> Self {
        Statement::Variable {
            mutable,
            name,
            type_annotation: None,
            value,
        }
    }
    
    pub fn constant(name: String, value: Expression) -> Self {
        Statement::Constant {
            name,
            type_annotation: None,
            value,
        }
    }
    
    pub fn function(name: String, parameters: Vec<Parameter>, body: Vec<Statement>) -> Self {
        Statement::Function {
            name,
            parameters,
            return_type: None,
            body,
            async_flag: false,
        }
    }
}