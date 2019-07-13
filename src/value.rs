use crate::parser::FunctionDef;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
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
                    Value::Array(b) => Rc::ptr_eq(&a.0, &b.0),
                    _ => false,
                }
            },
            Value::Object(a) => {
                match other {
                    Value::Object(b) => Rc::ptr_eq(&a.0, &b.0),
                    _ => false,
                }
            },
            Value::Function(a) => {
                match other {
                    Value::Function(b) => Rc::ptr_eq(&a.0, &b.0),
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
    pub fn string(string: String) -> Value {
        Value::String(ImmutableString::new(string))
    }

    pub fn function(def: Rc<FunctionDef>) -> Value {
        Value::Function(Function(def))
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Array(Rc::new(RefCell::new(values))))
    }

    pub fn object(values: BTreeMap<String, Value>) -> Value {
        Value::Object(Object(Rc::new(RefCell::new(values))))
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

#[derive(Clone, Debug)]
pub struct Function(Rc<FunctionDef>);

impl Function {
    pub fn def(&self) -> &FunctionDef {
        &*self.0
    }
}

#[derive(Clone, Debug)]
pub struct Array(Rc<RefCell<Vec<Value>>>);

impl Array {
    pub fn new() -> Array {
        Array(Rc::new(RefCell::new(Vec::new())))
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

#[derive(Clone, Debug)]
pub struct Object(Rc<RefCell<BTreeMap<String, Value>>>);

// TODO: Allow for storing hidden values accessible only in Rust.
impl Object {
    pub fn new() -> Object {
        Object(Rc::new(RefCell::new(BTreeMap::new())))
    }

    pub fn set(&self, key: String, value: Value) {
        self.0.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.0.borrow().get(key).cloned()
    }
}
