use crate::position::Position;
use crate::value::{Object, Value};

#[derive(Clone, Debug)]
pub enum Error {
    Runtime(Value),
    Lexer(LexerError),
    Parser(ParserError),
}

impl Error {
    pub fn redeclaration_of_variable(pos: Position) -> Value {
        Error::runtime(pos, "redeclaration of variable".into())
    }

    pub fn undeclared_variable(pos: Position) -> Value {
        Error::runtime(pos, "undeclared variable".into())
    }

    pub fn not_a_function(pos: Position) -> Value {
        Error::runtime(pos, "not a function".into())
    }

    pub fn bad_break(pos: Position) -> Value {
        Error::runtime(pos, "`break` used outside a loop".into())
    }

    pub fn bad_continue(pos: Position) -> Value {
        Error::runtime(pos, "`continue` used outside a loop".into())
    }

    pub fn bad_return(pos: Position) -> Value {
        Error::runtime(pos, "`return` used outside a function".into())
    }

    pub fn bad_iter(pos: Position) -> Value {
        Error::runtime(pos, "expected an iterator object with a `next` function".into())
    }

    pub fn bad_iter_next(pos: Position) -> Value {
        Error::runtime(pos, "expected iterator's `next` to return an object".into())
    }

    pub fn invalid_operands(pos: Position) -> Value {
        Error::runtime(pos, "invalid operands for operation".into())
    }

    pub fn invalid_operand(pos: Position) -> Value {
        Error::runtime(pos, "invalid operand for operation".into())
    }

    pub fn invalid_assign_lhs(pos: Position) -> Value {
        Error::runtime(pos, "invalid assignment left-hand side".into())
    }

    pub fn non_string_object_key(pos: Position) -> Value {
        Error::runtime(pos, "object key must be a string".into())
    }

    pub fn non_numeric_array_index(pos: Position) -> Value {
        Error::runtime(pos, "array index must be a number".into())
    }

    pub fn negative_array_index(pos: Position) -> Value {
        Error::runtime(pos, "array index cannot be negative".into())
    }

    pub fn invalid_indexee(pos: Position) -> Value {
        Error::runtime(pos, "only arrays and objects can be indexed".into())
    }

    pub fn invalid_dot(pos: Position) -> Value {
        Error::runtime(pos, "dot operator can only be used on objects".into())
    }

    pub fn runtime(pos: Position, message: String) -> Value {
        let object = Object::new();
        object.set("message".into(), Value::string(message));
        object.set("line".into(), Value::Number(pos.line as f64));
        object.set("column".into(), Value::Number(pos.column as f64));
        Value::Object(object)
    }
}

#[derive(Clone, Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub position: Position,
}

#[derive(Clone, Debug)]
pub enum LexerErrorKind {
    UnexpectedChar,
    MalformedNumber,
    MalformedEscapeSequence,
    MalformedChar,
}

#[derive(Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: Position,
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    ExpectedLCurly,
    ExpectedRParen,
    ExpectedSemicolon,
    UnexpectedToken,
    UnexpectedEOF,
}

impl From<Value> for Error {
    fn from(value: Value) -> Error {
        Error::Runtime(value)
    }
}
