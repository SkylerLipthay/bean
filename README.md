# Bean

Bean is a simple, dynamically typed, embedded scripting language that was built in the context of client-server web application scripting. Bean is syntactically a blend of Rust and JavaScript.

Bean is very much a work-in-progress, and so the following contents of this README serve primarily as a scratchpad of ideas.

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

* `String`s, `Number`s, `Boolean`s, `Null`s are passed by value.
* `String`s are interned (via `CowRc`).
* `Array`/`Object`/`Function` are passed by reference.

## Syntax sample

```
let do_some_math = |func, a, b, c| func(a, b) * c;

let result = do_some_math(|a, b| a + b, 3, 4, 5);
let some_nonsense = if result > 50 {
  "1"
} else if result >= 25 {
  2
} else {
  '3'
};

let some_object = {
  someKey: 1,
  "all keys must be strings": ["values", "can", "be", "whatever", null],
};
```

## Standard library

* Accessible using `std`.

```
let object = { a: 1, b: "two" };
std.len(object);
for key in std.keys(object) { /* "a", "b" */ }
for value in std.values(object) { /* 1, "two" */ }
for pair in std.iter(object) { /* ["a", 1], ["b", "two"] */ }
for i in std.range(0, 5) { /* 0, 1, 2, 3, 4 */ }
for i in std.range_inc(0, 5) { /* 0, 1, 2, 3, 4, 5 */ }
let array = [1, 2, 3];
std.len(array);
std.iter(array);
```

## Thoughts

* `Scope`s are layers of execution contexts that each contain a map between identifiers and values.
* When an identifier is being resolved, the engine traverses up the scope stack until it finds a value map containing the identifier.
* The `RootScope` is where standard library is implemented. Its values cannot be re-assigned and it is never removed from the scope stack.
* Running a script instantiates a new `BlockScope` that lives in the deepest level of the scope stack. Like all other `BlockScope`s, this scope is finally vaporized into a value.
* Consumers can create their own reusable `BlockScope`s so that variables can live from one script execution to another.
* There may also be `FunctionScope` and `LoopScope`, which support `return` and `break`/`continue` respectively.
* Perhaps scopes can be defined like `struct Scope { kind: ScopeKind, namespace: Object<'ctx> }`?

* Can't impose a memory allocation limitation on a script's execution context until Rust's custom allocator story is mature. (Unless I want to re-implement `String`, `Vec`, `BTreeMap`...)
