use crate::context::Context;
use crate::parser::FunctionBean;
use bacon_rajan_cc::{Cc, Trace, Tracer};
use std::collections::BTreeMap;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(ImmutableString),
    Boolean(bool),
    Array(Array),
    Object(Object),
    Function(Function),
    Null,
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match self {
            Value::Number(a) => {
                match other {
                    Value::Number(b) => a == b,
                    _ => false,
                }
            },
            Value::String(a) => {
                match other {
                    Value::String(b) => a == b,
                    _ => false,
                }
            },
            Value::Boolean(a) => {
                match other {
                    Value::Boolean(b) => a == b,
                    _ => false,
                }
            },
            Value::Array(a) => {
                match other {
                    Value::Array(b) => (&*a.0 as *const _) == (&*b.0 as *const _),
                    _ => false,
                }
            },
            Value::Object(a) => {
                match other {
                    Value::Object(b) => (&*a.0 as *const _) == (&*b.0 as *const _),
                    _ => false,
                }
            },
            Value::Function(a) => {
                match other {
                    Value::Function(b) => a == b,
                    _ => false,
                }
            },
            Value::Null => {
                match other {
                    Value::Null => true,
                    _ => false,
                }
            },
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{:?}", v),
            Value::Boolean(v) => write!(f, "{:?}", v),
            Value::Array(v) => write!(f, "{:?}", v),
            Value::Object(v) => write!(f, "{:?}", v),
            Value::Function(v) => write!(f, "{:?}", v),
            Value::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn string<S: Into<String>>(string: S) -> Value {
        Value::String(ImmutableString::new(string.into()))
    }

    pub fn bean_function(scopes: Vec<Object>, func: Rc<FunctionBean>) -> Value {
        Value::Function(Function { scopes, kind: FunctionKind::Bean(func) })
    }

    pub fn rust_function(func: FunctionRust, udata: Option<Value>) -> Value {
        Value::Function(Function {
            scopes: Vec::new(),
            kind: FunctionKind::Rust { func, udata: udata.map(|udata| Box::new(udata)) },
        })
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Array(Cc::new(RefCell::new(values))))
    }

    pub fn object(values: BTreeMap<String, Value>) -> Value {
        Value::Object(Object(Cc::new(RefCell::new(values))))
    }

    pub fn coerce_bool(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            Value::Null => false,
            _ => true,
        }
    }
}

impl Trace for Value {
    fn trace(&mut self, tracer: &mut Tracer) {
        match self {
            Value::Array(inner) => inner.trace(tracer),
            Value::Object(inner) => inner.trace(tracer),
            Value::Function(inner) => inner.trace(tracer),
            _ => {},
        };
    }
}

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ImmutableString(Rc<String>);

impl ImmutableString {
    pub fn new(string: String) -> ImmutableString {
        ImmutableString(Rc::new(string))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl fmt::Debug for ImmutableString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

#[derive(Clone)]
pub struct Function {
    // Unused for `FunctionKind::Rust` functions:
    scopes: Vec<Object>,
    kind: FunctionKind,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl PartialEq<Function> for Function {
    fn eq(&self, other: &Function) -> bool {
        match &self.kind {
            FunctionKind::Rust { func: fa, udata: va } => {
                match &other.kind {
                    FunctionKind::Rust { func: fb, udata: vb } => {
                        let same_fn = fa as *const _ == fb as *const _;
                        let same_val = va == vb;
                        same_fn && same_val
                    },
                    _ => false,
                }
            },
            FunctionKind::Bean(a) => {
                match &other.kind {
                    FunctionKind::Bean(b) => Rc::ptr_eq(&a, &b),
                    _ => false,
                }
            }
        }
    }
}

impl Trace for Function {
    fn trace(&mut self, tracer: &mut Tracer) {
        match self.kind {
            FunctionKind::Rust { ref mut udata, .. } => udata.trace(tracer),
            _ => {},
        };
    }
}

#[derive(Clone)]
pub enum FunctionKind {
    Rust {
        udata: Option<Box<Value>>,
        func: FunctionRust,
    },
    Bean(Rc<FunctionBean>),
}

pub type FunctionRust = fn(&mut Context, Option<Value>, Vec<Value>) -> Result<Value, Value>;

impl fmt::Debug for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionKind::Rust { .. } => write!(f, "<rust function>"),
            FunctionKind::Bean(_) => write!(f, "<bean function>"),
        }
    }
}

impl Function {
    pub fn scopes(&self) -> &[Object] {
        self.scopes.as_slice()
    }

    pub fn kind(&self) -> &FunctionKind {
        &self.kind
    }
}

#[derive(Clone)]
pub struct Array(Cc<RefCell<Vec<Value>>>);

impl Array {
    pub fn new() -> Array {
        Array(Cc::new(RefCell::new(Vec::new())))
    }

    pub fn set(&self, index: usize, value: Value) {
        let mut vec = self.0.borrow_mut();
        let len = vec.len();
        if index >= len {
            vec.reserve(index - len + 1);
            for _ in len..=index {
                vec.push(Value::Null);
            }
        }

        vec[index] = value;
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.0.borrow().get(index).cloned()
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}

impl Trace for Array {
    fn trace(&mut self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

#[derive(Clone)]
pub struct Object(Cc<RefCell<BTreeMap<String, Value>>>);

// TODO: Allow for storing hidden values accessible only in Rust.
impl Object {
    pub fn new() -> Object {
        Object(Cc::new(RefCell::new(BTreeMap::new())))
    }

    pub fn set<S: Into<String>>(&self, key: S, value: Value) {
        self.0.borrow_mut().insert(key.into(), value);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.0.borrow().get(key).cloned()
    }

    pub fn contains(&self, key: &str) -> bool {
        self.0.borrow().contains_key(key)
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}

impl Trace for Object {
    fn trace(&mut self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}
