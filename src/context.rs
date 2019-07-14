use crate::error::Error;
use crate::parser::{Block, Expr, ExprKind as EK, FunctionBean, Parser};
use crate::position::Position;
use crate::value::{ImmutableString, Function, FunctionRust, FunctionKind, Object, Value};
use std::collections::BTreeMap;
use std::mem;

pub struct Context {
    scopes: Vec<Object>,
}

macro_rules! loop_body {
    ($self:expr, $block:expr) => {
        loop_body!(@ $self, $block, eval_block)
    };

    (no_scope, $self:expr, $block:expr) => {
        loop_body!(@ $self, $block, eval_block_no_scope)
    };

    (@ $self:expr, $block:expr, $method:ident) => {
        match $self.$method($block) {
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
    };
}

macro_rules! op_infix {
    ($self:expr, $pos:expr, $a:expr, $b:expr, $cast:ty, $operator:tt) => {
        {
            #[inline]
            fn inner(
                context: &mut Context,
                pos: Position,
                a: &Expr,
                b: &Expr,
            ) -> Result<Value, Interrupt> {
                if let Value::Number(a) = context.eval_expr(a)? {
                    if let Value::Number(b) = context.eval_expr(b)? {
                        return Ok(Value::Number(((a as $cast) $operator (b as $cast)) as f64));
                    }
                }

                Err(Interrupt::Error(Error::invalid_operands(pos)))
            }

            inner($self, $pos, $a, $b)
        }
    };
}

macro_rules! op_assn {
    ($cast:ty, $operator:tt) => {
        |ctx, pos, lhs, rhs| op_infix!(ctx, pos, lhs, rhs, $cast, $operator)
    };
}

macro_rules! op_inequality {
    ($self:expr, $pos:expr, $a:expr, $b:expr, $operator:tt) => {
        {
            #[inline]
            fn inner(
                context: &mut Context,
                pos: Position,
                a: &Expr,
                b: &Expr,
            ) -> Result<Value, Interrupt> {
                let a = context.eval_expr(a)?;
                let b = context.eval_expr(b)?;

                if let Value::Number(a) = a {
                    if let Value::Number(b) = b {
                        return Ok(Value::Boolean(a $operator b));
                    }
                } else if let Value::String(ref a) = a {
                    if let Value::String(ref b) = b {
                        return Ok(Value::Boolean(a.as_str() $operator b.as_str()));
                    }
                }

                Err(Interrupt::Error(Error::invalid_operands(pos)))
            }

            inner($self, $pos, $a, $b)
        }
    };
}

macro_rules! op_prefix {
    // If `$operator` is not given, this just serves as an assertion that the operand is a number.
    ($self:expr, $pos:expr, $operand:expr, $cast:ty $(, $operator:tt)?) => {
        {
            #[inline]
            fn inner(
                context: &mut Context,
                pos: Position,
                expr: &Expr,
            ) -> Result<Value, Interrupt> {
                if let Value::Number(value) = context.eval_expr(expr)? {
                    return Ok(Value::Number(($($operator)? (value as $cast)) as f64));
                }

                Err(Interrupt::Error(Error::invalid_operand(pos)))
            }

            inner($self, $pos, $operand)
        }
    };
}

impl Context {
    pub fn new() -> Context {
        Context { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self, scope: Object) {
        self.scopes.push(scope);
    }

    pub fn eval(&mut self, source: &str) -> Result<Value, Error> {
        Ok(self.eval_block(&Parser::parse_script(source)?)?)
    }

    pub fn call_function(&mut self, func: &Function, vals: Vec<Value>) -> Result<Value, Value> {
        Ok(self.eval_call_inner_vals(func, vals)?)
    }

    fn eval_block(&mut self, block: &Block) -> Result<Value, Interrupt> {
        self.scopes.push(Object::new());
        let result = self.eval_block_no_scope(block);
        self.scopes.pop();
        result
    }

    fn eval_block_no_scope(&mut self, block: &Block) -> Result<Value, Interrupt> {
        let mut result = Ok(Value::Null);
        for expr in block.iter() {
            match self.eval_expr(expr) {
                Ok(value) => result = Ok(value),
                Err(err) => {
                    result = Err(err);
                    break;
                },
            };
        }
        result
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, Interrupt> {
        let Expr { kind, position } = expr;
        let pos = *position;
        match kind {
            EK::Continue(expr) => Err(self.eval_continue(pos, expr)),
            EK::Break(expr) => Err(self.eval_break(pos, expr)),
            EK::Return(expr) => Err(self.eval_return(pos, expr)),
            EK::Throw(expr) => Err(self.eval_throw(expr)),
            EK::Let(ident, value) => self.eval_let(pos, ident, value),

            EK::Identifier(ident) => self.eval_identifier(pos, ident),
            EK::Function(def) => Ok(Value::function(self.scopes.clone(), def.clone())),
            EK::Boolean(value) => Ok(Value::Boolean(*value)),
            EK::Number(value) => Ok(Value::Number(*value)),
            EK::String(value) => Ok(Value::string(value.clone())),
            EK::Array(array) => self.eval_array(array),
            EK::Object(array) => self.eval_object(array),
            EK::Null => Ok(Value::Null),
            EK::Paren(expr) => self.eval_expr(expr),
            EK::If(conditions, else_block) => self.eval_if(conditions, else_block),
            EK::Try(block, (ident, catch)) => self.eval_try(block, ident, catch),
            EK::While(condition, block) => self.eval_while(condition, block),
            EK::Loop(block) => self.eval_loop(block),
            EK::For(ident, iter, block) => self.eval_for(pos, ident, iter, block),
            EK::Block(block) => self.eval_block(block),

            EK::Call(func, args) => self.eval_call(pos, func, args),
            EK::Index(expr, key) => self.eval_index(pos, expr, key),
            EK::Dot(expr, ident) => self.eval_dot(pos, expr, ident),

            EK::Negative(expr) => op_prefix!(self, pos, expr, f64, -),
            EK::Positive(expr) => op_prefix!(self, pos, expr, f64),
            EK::BoolNot(expr) => Ok(Value::Boolean(!self.eval_expr(expr)?.coerce_bool())),
            EK::BitNegate(expr) => op_prefix!(self, pos, expr, u64, !),

            EK::Equal(a, b) => Ok(Value::Boolean(self.eval_expr(a)? == self.eval_expr(b)?)),
            EK::NotEqual(a, b) => Ok(Value::Boolean(self.eval_expr(a)? != self.eval_expr(b)?)),
            EK::LessThan(a, b) => op_inequality!(self, pos, a, b, <),
            EK::LessThanEqual(a, b) => op_inequality!(self, pos, a, b, <=),
            EK::GreaterThan(a, b) => op_inequality!(self, pos, a, b, >),
            EK::GreaterThanEqual(a, b) => op_inequality!(self, pos, a, b, >=),

            EK::Add(a, b) => self.eval_add(pos, a, b),
            EK::Subtract(a, b) => op_infix!(self, pos, a, b, f64, -),
            EK::Multiply(a, b) => op_infix!(self, pos, a, b, f64, *),
            EK::Divide(a, b) => op_infix!(self, pos, a, b, f64, /),
            EK::Modulo(a, b) => op_infix!(self, pos, a, b, f64, %),
            EK::LeftShift(a, b) => op_infix!(self, pos, a, b, u64, <<),
            EK::RightShift(a, b) => op_infix!(self, pos, a, b, u64, >>),
            EK::BoolOr(a, b) => self.eval_bool_or(a, b),
            EK::BoolXor(a, b) => self.eval_bool_xor(a, b),
            EK::BoolAnd(a, b) => self.eval_bool_and(a, b),
            EK::BitOr(a, b) => op_infix!(self, pos, a, b, u64, |),
            EK::BitAnd(a, b) => op_infix!(self, pos, a, b, u64, &),
            EK::BitXor(a, b) => op_infix!(self, pos, a, b, u64, ^),

            EK::Assign(l, r) => self.eval_assign(pos, l, r, |ctx, _, _, rhs| ctx.eval_expr(rhs)),
            EK::LeftShiftAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(u64, <<)),
            EK::RightShiftAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(u64, >>)),
            EK::PlusAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(f64, +)),
            EK::MinusAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(f64, -)),
            EK::MultiplyAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(f64, *)),
            EK::DivideAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(f64, /)),
            EK::ModuloAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(f64, %)),
            EK::OrAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(u64, |)),
            EK::AndAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(u64, &)),
            EK::XorAssign(l, r) => self.eval_assign(pos, l, r, op_assn!(u64, ^)),
        }
    }

    fn eval_dot(&mut self, pos: Position, expr: &Expr, ident: &String) -> Result<Value, Interrupt> {
        match self.eval_expr(expr)? {
            Value::Object(ref object) => Ok(object.get(ident).unwrap_or(Value::Null)),
            _ => Err(Interrupt::Error(Error::invalid_dot(pos))),
        }
    }

    fn eval_object_index(&mut self, pos: Position, e: &Expr) -> Result<ImmutableString, Interrupt> {
        match self.eval_expr(e)? {
            Value::String(ref key) => Ok(key.clone()),
            _ => Err(Interrupt::Error(Error::non_string_object_key(pos))),
        }
    }

    fn eval_array_index(&mut self, pos: Position, key: &Expr) -> Result<usize, Interrupt> {
        match self.eval_expr(key)? {
            Value::Number(index) if index >= 0.0 => Ok(index as usize),
            Value::Number(_) => Err(Interrupt::Error(Error::negative_array_index(pos))),
            _ => Err(Interrupt::Error(Error::non_numeric_array_index(pos))),
        }
    }

    fn eval_index(&mut self, pos: Position, expr: &Expr, key: &Expr) -> Result<Value, Interrupt> {
        match self.eval_expr(expr)? {
            Value::Object(ref object) => {
                Ok(object.get(&self.eval_object_index(pos, key)?.as_str()).unwrap_or(Value::Null))
            },
            Value::Array(ref array) => {
                Ok(array.get(self.eval_array_index(pos, key)?).unwrap_or(Value::Null))
            },
            _ => return Err(Interrupt::Error(Error::invalid_indexee(expr.position))),
        }
    }

    fn eval_assign<F>(
        &mut self,
        pos: Position,
        lhs: &Expr,
        rhs: &Expr,
        value_fn: F,
    ) -> Result<Value, Interrupt>
    where
        F: Fn(&mut Context, Position, &Expr, &Expr) -> Result<Value, Interrupt>,
    {
        match &lhs.kind {
            EK::Identifier(ident) => {
                let scope_index = self.resolve_scope_index(lhs.position, ident)?;
                let value = value_fn(self, pos, lhs, rhs)?;
                self.scopes[scope_index].set(ident.clone(), value.clone());
                Ok(value)
            },
            EK::Dot(expr, ident) => {
                match self.eval_expr(expr)? {
                    Value::Object(ref object) => {
                        let value = value_fn(self, pos, lhs, rhs)?;
                        object.set(ident.clone(), value.clone());
                        Ok(value)
                    },
                    _ => Err(Interrupt::Error(Error::invalid_dot(lhs.position))),
                }
            },
            EK::Index(expr, key) => {
                match self.eval_expr(expr)? {
                    Value::Object(ref object) => {
                        let key = self.eval_object_index(expr.position, key)?.to_string();
                        let value = value_fn(self, pos, lhs, rhs)?;
                        object.set(key, value.clone());
                        Ok(value)
                    },
                    Value::Array(ref array) => {
                        let index = self.eval_array_index(expr.position, key)?;
                        let value = value_fn(self, pos, lhs, rhs)?;
                        array.set(index, value.clone());
                        Ok(value)
                    },
                    _ => return Err(Interrupt::Error(Error::invalid_indexee(lhs.position))),
                }
            },
            _ => Err(Interrupt::Error(Error::invalid_assign_lhs(pos))),
        }
    }

    fn resolve_scope_index(&self, pos: Position, ident: &String) -> Result<usize, Interrupt> {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains(ident) {
                return Ok(index);
            }
        }

        Err(Interrupt::Error(Error::undeclared_variable(pos)))
    }

    fn eval_add(&mut self, pos: Position, a: &Expr, b: &Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?;
        let b = self.eval_expr(b)?;

        if let Value::Number(a) = a {
            if let Value::Number(b) = b {
                return Ok(Value::Number(a + b));
            }
        } else if let Value::String(ref a) = a {
            if let Value::String(ref b) = b {
                return Ok(Value::string(a.as_str().to_string() + b.as_str()));
            }
        }

        Err(Interrupt::Error(Error::invalid_operands(pos)))
    }

    fn eval_object(&mut self, object: &BTreeMap<String, Expr>) -> Result<Value, Interrupt> {
        let mut result = BTreeMap::new();

        for (ident, expr) in object {
            result.insert(ident.clone(), self.eval_expr(expr)?);
        }

        Ok(Value::object(result))
    }

    fn eval_array(&mut self, array: &Vec<Expr>) -> Result<Value, Interrupt> {
        let mut result = Vec::with_capacity(array.len());

        for expr in array {
            result.push(self.eval_expr(expr)?);
        }

        Ok(Value::array(result))
    }

    fn eval_throw(&mut self, expr: &Expr) -> Interrupt {
        match self.eval_expr(expr) {
            Ok(value) => Interrupt::Error(value),
            Err(err) => err,
        }
    }

    fn flow_value(&mut self, expr: &Option<Box<Expr>>) -> Result<Option<Value>, Interrupt> {
        match expr {
            Some(expr) => {
                match self.eval_expr(expr) {
                    Ok(value) => Ok(Some(value)),
                    Err(err) => Err(err),
                }
            },
            None => Ok(None),
        }
    }

    fn eval_continue(&mut self, position: Position, expr: &Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Continue { position, value },
            Err(err) => err,
        }
    }

    fn eval_break(&mut self, position: Position, expr: &Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Break { position, value },
            Err(err) => err,
        }
    }

    fn eval_return(&mut self, position: Position, expr: &Option<Box<Expr>>) -> Interrupt {
        match self.flow_value(expr) {
            Ok(value) => Interrupt::Return { position, value },
            Err(err) => err,
        }
    }

    fn eval_loop(&mut self, block: &Block) -> Result<Value, Interrupt> {
        loop {
            loop_body!(self, block);
        }
    }

    fn eval_for(
        &mut self,
        position: Position,
        ident: &String,
        iter: &Expr,
        block: &Block,
    ) -> Result<Value, Interrupt> {
        #[inline]
        fn inner(context: &mut Context, block: &Block) -> Result<Value, Interrupt> {
            Ok(loop_body!(no_scope, context, block))
        }

        let mut result = Value::Null;

        let iter_expr = self.eval_expr(iter)?;
        let iter = match iter_expr {
            Value::Object(ref iter) => iter,
            _ => return Err(Interrupt::Error(Error::bad_iter(position))),
        };

        let next_val = iter.get("next");
        let next = match next_val {
            Some(Value::Function(ref next)) => next,
            _ => return Err(Interrupt::Error(Error::bad_iter(position))),
        };

        let empty_args = Vec::new();
        loop {
            let object_val = self.eval_call_inner_exprs(next, &empty_args)?;
            let object = match object_val {
                Value::Object(ref object) => object,
                _ => return Err(Interrupt::Error(Error::bad_iter_next(position))),
            };

            let value = object.get("value").unwrap_or(Value::Null);

            if object.get("done").unwrap_or(Value::Null).coerce_bool() {
                break;
            }

            let scope = Object::new();
            scope.set(ident.clone(), value);
            self.scopes.push(scope);
            let inner_result = inner(self, block);
            self.scopes.pop();
            result = inner_result?;
        }

        Ok(result)
    }

    fn eval_while(&mut self, condition: &Expr, block: &Block) -> Result<Value, Interrupt> {
        let mut result = Value::Null;

        while self.eval_expr(condition)?.coerce_bool() {
            result = loop_body!(self, block);
        }

        Ok(result)
    }

    fn eval_try(
        &mut self,
        block: &Block,
        ident: &String,
        catch: &Block,
    ) -> Result<Value, Interrupt> {
        let err = match self.eval_block(block) {
            Ok(value) => return Ok(value),
            Err(Interrupt::Error(err)) => err,
            Err(interrupt) => return Err(interrupt),
        };

        let scope = Object::new();
        scope.set(ident.clone(), err);
        self.scopes.push(scope);
        let result = self.eval_block_no_scope(catch);
        self.scopes.pop();
        result
    }

    fn eval_if(
        &mut self,
        conditions: &Vec<(Expr, Block)>,
        else_block: &Option<Block>,
    ) -> Result<Value, Interrupt> {
        for (condition, block) in conditions.iter() {
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
        func: &Expr,
        args: &Vec<Expr>,
    ) -> Result<Value, Interrupt> {
        // TODO: Errors originating in called function have misleading line/column numbers if the
        // function was not defined in the current script. May have to accept a filename parameter
        // to disambiguate, or come up with another solution. Perhaps if no filename is supplied,
        // the error is "localized" to the caller's location (i.e. the line/column point to `pos`).
        //
        // Moreover, errors produced by native Rust functions are never tagged with a line/column.
        // Can this be improved?
        let func = self.eval_expr(func)?;
        if let Value::Function(ref func) = func {
            self.eval_call_inner_exprs(func, args)
        } else {
            Err(Interrupt::Error(Error::not_a_function(pos)))
        }
    }

    fn eval_call_inner_exprs(
        &mut self,
        func: &Function,
        args: &Vec<Expr>,
    ) -> Result<Value, Interrupt> {
        let mut vals = Vec::with_capacity(args.len());
        for arg in args {
            vals.push(self.eval_expr(arg)?);
        }

        self.eval_call_inner_vals(func, vals)
    }

    fn eval_call_inner_vals(
        &mut self,
        func: &Function,
        vals: Vec<Value>,
    ) -> Result<Value, Interrupt> {
        match func.kind() {
            FunctionKind::Bean(bean_func) => self.eval_call_bean(bean_func, func.scopes(), vals),
            FunctionKind::Rust(rust_func) => self.eval_call_rust(rust_func, vals),
        }
    }

    fn eval_call_bean(
        &mut self,
        func: &FunctionBean,
        scopes: &[Object],
        mut vals: Vec<Value>,
    ) -> Result<Value, Interrupt> {
        let scope = Object::new();
        for param in func.params.iter() {
            let val = if vals.len() > 0 {
                vals.remove(0)
            } else {
                Value::Null
            };

            scope.set(param.clone(), val);
        }

        // Restore the original scope stack at the declaration site of the function:
        let ctx_scopes = mem::replace(&mut self.scopes, scopes.to_vec());
        self.scopes.push(scope);

        let result = match self.eval_expr(&func.body) {
            Ok(value) => Ok(value),
            Err(Interrupt::Return { value, .. }) => Ok(match value {
                Some(value) => value,
                None => Value::Null,
            }),
            Err(Interrupt::Continue { position, .. }) => {
                Err(Interrupt::Error(Error::bad_continue(position)))
            },
            Err(Interrupt::Break { position, .. }) => {
                Err(Interrupt::Error(Error::bad_break(position)))
            },
            Err(err) => Err(err),
        };

        self.scopes = ctx_scopes;

        result
    }

    fn eval_call_rust(
        &mut self,
        func: &FunctionRust,
        vals: Vec<Value>,
    ) -> Result<Value, Interrupt> {
        func(self, vals).map_err(|val| Interrupt::Error(val))
    }

    fn eval_bool_or(&mut self, a: &Expr, b: &Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        if a {
            // Short-circuit evaluation.
            Ok(Value::Boolean(true))
        } else {
            let b = self.eval_expr(b)?.coerce_bool();
            Ok(Value::Boolean(a || b))
        }
    }

    fn eval_bool_xor(&mut self, a: &Expr, b: &Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        let b = self.eval_expr(b)?.coerce_bool();
        Ok(Value::Boolean(!a != !b))
    }

    fn eval_bool_and(&mut self, a: &Expr, b: &Expr) -> Result<Value, Interrupt> {
        let a = self.eval_expr(a)?.coerce_bool();
        if !a {
            // Short-circuit evaluation.
            Ok(Value::Boolean(false))
        } else {
            let b = self.eval_expr(b)?.coerce_bool();
            Ok(Value::Boolean(a && b))
        }
    }

    fn eval_identifier(&mut self, pos: Position, ident: &String) -> Result<Value, Interrupt> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(ident) {
                return Ok(value.clone());
            }
        }

        Err(Interrupt::Error(Error::undeclared_variable(pos)))
    }

    fn eval_let(
        &mut self,
        pos: Position,
        ident: &String,
        expr: &Option<Box<Expr>>,
    ) -> Result<Value, Interrupt> {
        if self.top_scope().contains(ident) {
            return Err(Interrupt::Error(Error::redeclaration_of_variable(pos)));
        }

        let value = match expr {
            Some(expr) => self.eval_expr(expr)?,
            None => Value::Null,
        };

        self.top_scope().set(ident.clone(), value);

        Ok(Value::Null)
    }

    #[inline]
    fn top_scope(&self) -> &Object {
        self.scopes.last().expect("scope stack unexpectedly empty")
    }
}

pub enum Interrupt {
    Break { value: Option<Value>, position: Position },
    Continue { value: Option<Value>, position: Position },
    Return { value: Option<Value>, position: Position },
    Error(Value),
}

impl From<Interrupt> for Error {
    fn from(interrupt: Interrupt) -> Error {
        let value: Value = interrupt.into();
        Error::Runtime(value)
    }
}

impl From<Interrupt> for Value {
    fn from(interrupt: Interrupt) -> Value {
        match interrupt {
            Interrupt::Break { position, .. } => Error::bad_break(position),
            Interrupt::Continue { position, .. } => Error::bad_continue(position),
            Interrupt::Return { position, .. } => Error::bad_return(position),
            Interrupt::Error(value) => value,
        }
    }
}
