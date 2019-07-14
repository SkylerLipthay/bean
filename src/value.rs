use crate::context::Context;
use crate::parser::FunctionBean;
use gc::{Gc, GcCell};
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, Trace, Finalize)]
pub enum Value {
    Number(f64),
    String(#[unsafe_ignore_trace] ImmutableString),
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
                    Value::Function(b) => Rc::ptr_eq(&a.kind, &b.kind),
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

impl Value {
    pub fn string<S: Into<String>>(string: S) -> Value {
        Value::String(ImmutableString::new(string.into()))
    }

    pub fn function(scopes: Vec<Object>, kind: Rc<FunctionKind>) -> Value {
        Value::Function(Function { scopes, kind })
    }

    pub fn rust_function<F>(func: F) -> Value
    where
        F: Fn(&mut Context, Vec<Value>) -> Result<Value, Value> + 'static,
    {
        Value::Function(Function {
            scopes: Vec::new(),
            kind: Rc::new(FunctionKind::Rust(Box::new(func))),
        })
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Array(Gc::new(GcCell::new(values))))
    }

    pub fn object(values: BTreeMap<String, Value>) -> Value {
        Value::Object(Object(Gc::new(GcCell::new(values))))
    }

    pub fn coerce_bool(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            Value::Null => false,
            _ => true,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

#[derive(Clone, Trace, Finalize)]
pub struct Function {
    // Unused for `FunctionKind::Rust` functions:
    scopes: Vec<Object>,
    #[unsafe_ignore_trace]
    kind: Rc<FunctionKind>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")
    }
}

pub enum FunctionKind {
    Rust(FunctionRust),
    Bean(FunctionBean),
}

pub type FunctionRust = Box<Fn(&mut Context, Vec<Value>) -> Result<Value, Value>>;

impl fmt::Debug for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")
    }
}

impl Function {
    pub fn scopes(&self) -> &[Object] {
        self.scopes.as_slice()
    }

    pub fn kind(&self) -> &FunctionKind {
        &*self.kind
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct Array(Gc<GcCell<Vec<Value>>>);

impl Array {
    pub fn new() -> Array {
        Array(Gc::new(GcCell::new(Vec::new())))
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
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct Object(Gc<GcCell<BTreeMap<String, Value>>>);

// TODO: Allow for storing hidden values accessible only in Rust.
impl Object {
    pub fn new() -> Object {
        Object(Gc::new(GcCell::new(BTreeMap::new())))
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
}
