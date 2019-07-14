use crate::value::{Object, Value};

pub fn scope() -> Object {
    let scope = Object::new();
    let std = Object::new();

    std.set("len", Value::rust_function(|_ctx, args| {
        match args.get(0) {
            Some(Value::Array(arr)) => Ok(Value::Number(arr.len() as f64)),
            Some(Value::Object(obj)) => Ok(Value::Number(obj.len() as f64)),
            _ => Err(Value::string("invalid argument")),
        }
    }));

    std.set("iter", Value::rust_function(|_ctx, args| {
        match args.get(0) {
            Some(Value::Array(arr)) => {
                let iter = Object::new();
                let iter_clone = iter.clone();
                // TODO: Can `args` be destructured to prevent this clone?
                let arr = arr.clone();

                // TODO: Use internal object value for this:
                iter.set("_index", Value::Number(0.0));

                // TODO: Bad news, gamers. https://github.com/Manishearth/rust-gc/issues/50
                iter.set("next", Value::rust_function(move |_ctx, _args| {
                    let next = Object::new();
                    let index = match iter_clone.get("_index") {
                        Some(Value::Number(number)) => number as usize,
                        _ => panic!("TODO: Use internal object value for this"),
                    };
                    if let Some(value) = arr.get(index) {
                        iter_clone.set("_index", Value::Number((index + 1) as f64));
                        next.set("value", value);
                    } else {
                        next.set("done", Value::Boolean(true));
                    }
                    Ok(Value::Object(next))
                }));

                Ok(Value::Object(iter))
            },
            _ => Err(Value::string("invalid argument")),
        }
    }));

    scope.set("std", Value::Object(std));
    scope
}
