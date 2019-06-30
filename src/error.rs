use crate::position::Position;

#[derive(Copy, Clone, Debug)]
pub enum Error {
    // TODO: Runtime errors. These can be laced in and out of Bean and Rust. I guess they should
    // just be `Value`s.
    Lexer(LexerError),
    Parser(ParserError),
}

#[derive(Copy, Clone, Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub position: Position,
}

#[derive(Copy, Clone, Debug)]
pub enum LexerErrorKind {
    UnexpectedChar,
    MalformedNumber,
    MalformedEscapeSequence,
    MalformedChar,
}

#[derive(Copy, Clone, Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: Position,
}

#[derive(Copy, Clone, Debug)]
pub enum ParserErrorKind {
    ExpectedLCurly,
    ExpectedRParen,
    ExpectedSemicolon,
    UnexpectedToken,
    UnexpectedEOF,
}
