`permutate-macro`

A procedural macro for permutating a function at compile time.

For example:

```
#[permutate(
    parameters = {
        a: on | off,
        b: on | off,
        c: on | off
    },
    permutations = [
        (on, on, on),
        (off, on, off),
        (off, off, off)
    ]
)]
fn foo() {
    #[permutate(a = on)]
    println!("A");

    #[permutate(b = on)]
    println!("B");

    #[permutate(c = on)]
    println!("C");
}
```

Would expand to:

```
fn foo__on__on__on() {
    println!("A");
    println!("B");
    println!("C");
}

fn foo__off__on__off() {
    println!("B");
}

fn foo__off__off__off() {}
```

In addition, permutations can be specified by file path, or an environment variable containing a file path:

```
#[permutate(
    parameters = {
        foo: on | off,
        bar: on | off
    },
    permutations = [
        file("permutations.json", "path::to::this::module"),
        env("SOME_ENV_VAR", "path::to::this::module")
    ],
)]
fn func() {}
```

The expected format of this file is as follows:

```
{
    "path::to::this::module::func": [
        ["on", "on"],
        ["off", "on"],
        ["off", "off"],
    ],
    ...
}
```
