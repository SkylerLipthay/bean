use crate::value::{Object, Value};

pub fn scope() -> Object {
    let scope = Object::new();
    let std = Object::new();

    std.set("len", Value::rust_function(|_ctx, _udata, args| {
        match args.get(0) {
            Some(Value::Array(arr)) => Ok(Value::Number(arr.len() as f64)),
            Some(Value::Object(obj)) => Ok(Value::Number(obj.len() as f64)),
            _ => Err(Value::string("invalid argument")),
        }
    }, None));

    // TODO: This whole thing is nuts and demonstrates how ugly the current interop API is.
    std.set("iter", Value::rust_function(|_ctx, _udata, args| {
        match args.get(0) {
            Some(Value::Array(arr)) => {
                let iter = Object::new();

                // TODO: Use internal object value for these:
                iter.set("_index", Value::Number(0.0));
                // TODO: Can `args` be destructured to prevent this clone?
                iter.set("_arr", Value::Array(arr.clone()));

                // TODO: Closure captures `Cc`s:
                iter.set("next", Value::rust_function(move |_ctx, udata, _args| {
                    let iter = match udata {
                        Some(Value::Object(ref iter)) => iter,
                        _ => panic!("TODO: Wow this is gross"),
                    };

                    let next = Object::new();

                    let index = match iter.get("_index") {
                        Some(Value::Number(number)) => number as usize,
                        _ => panic!("TODO: Use internal object value for this"),
                    };
                    let arr = match iter.get("_arr") {
                        Some(Value::Array(ref arr)) => arr.clone(),
                        _ => panic!("TODO: Use internal object value for this"),
                    };

                    if let Some(value) = arr.get(index) {
                        iter.set("_index", Value::Number((index + 1) as f64));
                        next.set("value", value);
                    } else {
                        next.set("done", Value::Boolean(true));
                    }
                    Ok(Value::Object(next))
                }, Some(Value::Object(iter.clone()))));

                Ok(Value::Object(iter))
            },
            _ => Err(Value::string("invalid argument")),
        }
    }, None));

    scope.set("std", Value::Object(std));
    scope
}
