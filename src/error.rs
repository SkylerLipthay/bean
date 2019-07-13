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

    pub fn runtime(pos: Position, message: String) -> Value {
        let object = Object::new();
        object.insert("message".into(), Value::string(message));
        object.insert("line".into(), Value::Number(pos.line as f64));
        object.insert("column".into(), Value::Number(pos.column as f64));
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
