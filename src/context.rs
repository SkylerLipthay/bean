use crate::error::Error;
use crate::parser::{Block, Expr, ExprKind as EK, Parser};
use crate::position::Position;
use crate::value::Value;
use std::collections::BTreeMap;

pub struct Context {
    scopes: Vec<Scope>,
}

macro_rules! loop_body {
    ($self:expr, $block:expr) => {
        match $self.eval_block($block.clone()) {
            Ok(value) => value,
            Err(Interrupt::Continue { value, .. }) => match value {
                Some(value) => value,
                None => Value::Null,
            },
            Err(Interrupt::Break { value, .. }) => return Ok(match value {
                Some(value) => value,
                None => Value::Null,
            }),
            Err(err) => return Err(err),
        }
    }
}

impl Context {
    pub fn new() -> Context {
        Context { scopes: Vec::new() }
    }

    pub fn eval(&mut self, source: &str) -> Result<Value, Error> {
        Ok(self.eval_block(Parser::parse_script(source)?)?)
    }

    fn eval_block(&mut self, block: Block) -> Result<Value, Interrupt> {
        self.scopes.push(Scope::new());
        let mut result = Ok(Value::Null);
        for expr in block.into_iter() {
            match self.eval_expr(expr) {
                Ok(value) => result = Ok(value),
                Err(err) => {
                    result = Err(err);
                    break;
                },
            };
        }
        self.scopes.pop();
        result
    }

    fn eval_expr(&mut self, expr: Expr) -> Result<Value, Interrupt> {
        let Expr { kind, position: pos } = expr;
        match kind {
            EK::Continue(expr) => Err(self.eval_continue(pos, expr)),
            EK::Break(expr) => Err(self.eval_break(pos, expr)),
            EK::Return(expr) => Err(self.eval_return(pos, expr)),
            EK::Throw(expr) => Err(self.eval_throw(*expr)),
            EK::Let(ident, value) => self.eval_let(pos, ident, value),

            EK::Identifier(ident) => self.eval_identifier(pos, ident),
            EK::Function(def) => Ok(Value::function(def)),
            EK::Number(value) => Ok(Value::Number(value)),
            EK::String(value) => Ok(Value::string(value)),
            // Array
            // Object
            EK::Paren(expr) => self.eval_expr(*expr),
            EK::True => Ok(Value::Boolean(true)),
            EK::False => Ok(Value::Boolean(false)),
            EK::Null => Ok(Value::Null),
            EK::If(conditions, else_block) => self.eval_if(conditions, else_block),
            // Try
            EK::While(condition, block) => self.eval_while(*condition, block),
            EK::Loop(block) => self.eval_loop(block),
            // For
            EK::Block(block) => self.eval_block(block),

            EK::Call(func, args) => self.eval_call(pos, *func, args),
            // Index
            // Dot

            // Negative
            // Positive
            EK::BoolNot(expr) => Ok(Value::Boolean(!self.eval_expr(*expr)?.coerce_bool())),
            // BitNegate

            EK::Equal(a, b) => Ok(Value::Boolean(self.eval_expr(*a)? == self.eval_expr(*b)?)),
            EK::NotEqual(a, b) => Ok(Value::Boolean(self.eval_expr(*a)? != self.eval_expr(*b)?)),
            // LessThan
            // LessThanEqual
            // GreaterThan
            // GreaterThanEqual

            // Add
            // Subtract
            // Multiply
            // Divide
            // Modulo
            // LeftShift
            // RightShift
            EK::BoolOr(a, b) => self.eval_bool_or(*a, *b),
            EK::BoolXor(a, b) => self.eval_bool_xor(*a, *b),
            EK::BoolAnd(a, b) => self.eval_bool_and(*a, *b),
            // BitOr
            // BitAnd
            // BitXor

            // Assign
            // LeftShiftAssign
            // RightShiftAssign
            // PlusAssign
            // MinusAssign
            // MultiplyAssign
            // DivideAssign
            // ModuloAssign
            // OrAssign
            // AndAssign
            // XorAssign

            _ => unimplemented!(),
        }
    }

    fn eval_throw(&mut self, expr: Expr) -> Interrupt {
        match self.eval_expr(expr) {
            Ok(value) => Interrupt::Throw(value),
            Err(err) => err,
        }
    }

    fn flow_value(&mut self, expr: Option<Box<Expr>>) -> Result<Option<Value>, Interrupt> {
        match expr {
            Some(expr) => {
                match self.eval_expr(*expr) {
                    Ok(value) => Ok(Some(value)),
                    Err(err) => Err(err),
                }
            },
            None => Ok(None),
        }
    }

    fn eval_continue(&mut self, position: Position, expr: Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Continue { position, value },
            Err(err) => err,
        }
    }

    fn eval_break(&mut self, position: Position, expr: Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Break { position, value },
            Err(err) => err,
        }
    }

    fn eval_return(&mut self, position: Position, expr: Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Return { position, value },
            Err(err) => err,
        }
    }

    fn eval_loop(&mut self, block: Block) -> Result<Value, Interrupt> {
        loop {
            loop_body!(self, block);
        }
    }

    fn eval_while(&mut self, condition: Expr, block: Block) -> Result<Value, Interrupt> {
        let mut result = Value::Null;

        while self.eval_expr(condition.clone())?.coerce_bool() {
            result = loop_body!(self, block);
        }

        Ok(result)
    }

    fn eval_if(
        &mut self,
        conditions: Vec<(Expr, Block)>,
        else_block: Option<Block>,
    ) -> Result<Value, Interrupt> {
        for (condition, block) in conditions.into_iter() {
            if self.eval_expr(condition)?.coerce_bool() {
                return self.eval_block(block);
            }
        }

        match else_block {
            Some(block) => self.eval_block(block),
            None => Ok(Value::Null),
        }
    }

    fn eval_call(
        &mut self,
        pos: Position,
        func: Expr,
        mut args: Vec<Expr>,
    ) -> Result<Value, Interrupt> {
        // TODO: Errors originating in called function have misleading line/column numbers if the
        // function was not defined in the current script. May have to accept a filename parameter
        // to disambiguate, or come up with another solution. Perhaps if no filename is supplied,
        // the error is "localized" to the caller's location (i.e. the line/column point to `pos`).
        let func = self.eval_expr(func)?;
        if let Value::Function(func) = func {
            let mut scope = Scope::new();
            for param in func.def().params.iter() {
                let arg = match args.pop() {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Null,
                };
                scope.locals.insert(param.clone(), arg);
            }
            self.scopes.push(scope);
            let result = match self.eval_expr(*func.def().body.clone()) {
                Ok(value) => Ok(value),
                Err(Interrupt::Return { value, .. }) => Ok(match value {
                    Some(value) => value,
                    None => Value::Null,
                }),
                Err(err) => Err(err),
            };
            self.scopes.pop();
            result
        } else {
            Err(Interrupt::Error(Error::not_a_function(pos)))
        }
    }

    fn eval_bool_or(&mut self, a: Expr, b: Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        if a {
            // Short-circuit evaluation.
            Ok(Value::Boolean(true))
        } else {
            let b = self.eval_expr(b)?.coerce_bool();
            Ok(Value::Boolean(a || b))
        }
    }

    fn eval_bool_xor(&mut self, a: Expr, b: Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        let b = self.eval_expr(b)?.coerce_bool();
        Ok(Value::Boolean(!a != !b))
    }

    fn eval_bool_and(&mut self, a: Expr, b: Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        if !a {
            // Short-circuit evaluation.
            Ok(Value::Boolean(false))
        } else {
            let b = self.eval_expr(b)?.coerce_bool();
            Ok(Value::Boolean(a && b))
        }
    }

    fn eval_identifier(&mut self, pos: Position, ident: String) -> Result<Value, Interrupt> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.locals.get(&ident) {
                return Ok(value.clone());
            }
        }

        Err(Interrupt::Error(Error::undeclared_variable(pos)))
    }

    fn eval_let(
        &mut self,
        pos: Position,
        ident: String,
        value: Option<Box<Expr>>,
    ) -> Result<Value, Interrupt> {
        if self.top_scope_mut().locals.contains_key(&ident) {
            return Err(Interrupt::Error(Error::redeclaration_of_variable(pos)));
        }

        let value = match value {
            Some(expr) => self.eval_expr(*expr)?,
            None => Value::Null,
        };

        self.top_scope_mut().locals.insert(ident, value);

        Ok(Value::Null)
    }

    #[inline]
    fn top_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("scope stack unexpectedly empty")
    }
}

pub struct Scope {
    pub locals: BTreeMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { locals: BTreeMap::new() }
    }
}

pub enum Interrupt {
    Break { value: Option<Value>, position: Position },
    Continue { value: Option<Value>, position: Position },
    Return { value: Option<Value>, position: Position },
    Throw(Value),
    Error(Value),
}

impl From<Interrupt> for Error {
    fn from(interrupt: Interrupt) -> Error {
        match interrupt {
            Interrupt::Break { position, .. } => Error::Runtime(Error::bad_break(position)),
            Interrupt::Continue { position, .. } => Error::Runtime(Error::bad_continue(position)),
            Interrupt::Return { position, .. } => Error::Runtime(Error::bad_return(position)),
            Interrupt::Throw(value) => Error::Runtime(value),
            Interrupt::Error(value) => Error::Runtime(value),
        }
    }
}
