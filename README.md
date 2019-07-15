# Bean

Bean is a simple, dynamically typed, garbage collected, embedded scripting language that was built in the context of client-server web application scripting. The goal is to support server-side execution of untrusted, client-generated scripts. Bean is syntactically a blend of Rust and JavaScript.

This project is very much immature. Production usage is heavily discouraged.

To play around, clone this repository and run `cargo run --release --example repl`!

## Sample

```rust
// Variables are declared using `let`.
let x = 12.34;

// Functions are written similar to how Rust closures are written. Functions
// capture their scope, just like in JavaScript.
let multiply_x = |factor| x *= factor;
multiply_x(2);
// `x` is now `24.68`.

// Bean supports arrays.
let array = [1, true];
array[1]; // `true`
array[2]; // `null`
array[0] += 2;
array[4] = "!";
// Now, `array` is `[3, true, null, null, "!"]`.

// Bean supports objects (i.e. associative arrays). Object keys must be strings.
let object = { a: 1, ":)": true };
object.a; // `1`
object[":)"]; // `true`
object.b = 2;
object[":("] = false;

// Taking inspiration from Rust, almost nothing is a pure statement in Bean.
let x_status = if x < 10 {
  "less than 10"
} else if x > 20 {
  "greater than 20"
} else {
  "somewhere in the between"
};
// `x_status` is `"greater than 20"`

let prev_x = loop {
  x *= 2;
  if x > 80 {
    // The `break` and `continue` operators accept values! If no value is
    // provided, the loop evaluates to `null`.
    break x / 2;
  }
};
// `prev_x` is `49.36`.
// `x` is `98.72`.

while x > 20 {
  x /= 2;
}
// `x` is back to `12.34`.

// `for..in` is taken from Rust, but inspired by JavaScript's iterator protocol.
let sum = 0;
for i in std.iter([1, 2, 3]) {
  sum += i;
}
// `sum` is `6`.

// You can create your own iterators.
let double_each = |array| {
  let index = 0;
  {
    next: || if index >= std.len(array) {
      { done: true }
    } else {
      { value: array[index] * 2 }
    }
  }
};

for i in double_each([1, 2, 3]) { /* 2, 4, 6 */ }

// Functions can be higher-order.
let map_in_place = |array, callback| {
  let i = 0;
  while i < std.len(array) {
    array[i] = callback(array[i]);
    i += 1;
  }
};
let numbers = [1, 2, 3];
map_in_place(numbers, |n| n + 2);
// `numbers` is now `[3, 4, 5]`.

// `try`/`catch` can catch runtime exceptions thrown by the language itself.
try {
  123()
} catch e {
  // `e.message` is "not a function".
}

// And, of course, you can throw your own exceptions.
let may_throw_exception = |should_throw| {
  if should_throw {
    // Exceptions can be of any type.
    throw "uh-oh!";
  }
};

let result = try {
  may_throw_exception(true);
} catch e {
  e + " ...nevermind!"
};
// `result` is `"uh-oh! ...nevermind!"`
```

## Types

There are a limited number of value types.

* Number
  * IEEE 754 64-bit floats
* String
  * UTF-8
* Boolean
* Array
  * Elements can be of any type
* Object
  * Keys must be strings
  * Values can be of any type
* Null
* Function

Types are either passed to functions by value (string, number, boolean, null) or by reference (array, object, function).

## Standard library

Bean provides built-in functionality accessed through the `std` object.

* `std.len(<array>)`
* `std.len(<object>)`
* `std.iter(<array>)`

...The standard library needs a lot of work! It should eventually include all of the usual functionality to operate on all of the types.

## Prior art

Many thanks to [Rhai](https://github.com/jonathandturner/rhai) and the other [Rust-based scripting languages](https://github.com/rust-unofficial/awesome-rust#scripting) for inspiration!

## Future work

* The idea of running untrusted code is currently a pipe dream. It requires at least two measures:
  * Execution timeout mechanism. This is entirely possible to implement today.
  * Memory allocation limitation. Significantly more difficult than the former. At the very least it would require custom implementations of `BTreeMap`, `Vec`, and `String` (as of writing, Rust's type-level custom allocator story is missing).
* More ergonomic Rust-Bean bridging via capturing closures would be nice. As it stands, `rust-gc` has no way of tracing into closures.
* String interning would be nice.
* JIT would be nice.
* Eventually it'd be nice to have an analog to JavaScript's `this` for function calls. Along with this, function calls on built-ins would be nice, so as to facilitate e.g. `array.iter().sum()` in place of `std.sum(std.iter(array))`.
