//! Procedural macro for permutating functions at compile time.

#![feature(proc_macro_span)]

extern crate proc_macro;

mod permutate;

use proc_macro::TokenStream;

use syn::parse_macro_input;

/// Generates conditional permutations of a function based on a supplied set of parameters.
///
/// ```rust
/// # use permutate_macro::permutate;
/// #[permutate(
///    parameters = {
///        a: on | off,
///        b: on | off,
///        c: on | off
///    },
///    constants = {},
///    permutations = [
///        {
///            parameters = [
///                on,
///                on,
///                on
///            ],
///            constants = {}
///        },
///        {
///            parameters = [
///                off,
///                on,
///                off
///            ],
///            constants = {}
///        },
///        {
///            parameters = [
///                off,
///                off,
///                off
///            ],
///            constants = {}
///        }
///    ]
/// )]
/// fn foo() {
///    #[permutate(a = on)]
///    println!("A");
/// 
///    #[permutate(b = on)]
///    println!("B");
/// 
///    #[permutate(c = on)]
///    println!("C");
/// }
/// ```
/// 
/// Would expand to:
/// 
/// ```rust
/// fn foo__on__on__on() {
///     println!("A");
///     println!("B");
///     println!("C");
/// }
/// 
/// fn foo__off__on__off() {
///     println!("B");
/// }
/// 
/// fn foo__off__off__off() {}
/// ```
/// 
/// Key-value constants can also be specified, and injected in place of generic parameters:
/// 
/// ```rust
/// # use permutate_macro::permutate;
///
/// fn const_print<const VAL: u32>() {
///     println!("{VAL:}");
/// }
/// 
/// #[permutate(
///     parameters = {
///         a: on | off,
///         b: on | off
///     },
///    constants = {
///        CONST_INT: u32,
///    },
///    permutations = [
///        {
///            parameters = [
///                on,
///                off
///            ],
///            constants = {
///                CONST_INT = 4
///            }
///        },
///        {
///            parameters = [
///                off,
///                on
///            ],
///            constants = {
///                CONST_INT = 8
///            }
///        }
///    ]
/// )]
/// fn bar() {
///    #[permutate(a = on)]
///    {
///        println!("A:");
///        const_print::<permutate!(CONST_INT)>();
///    }
/// 
///    #[permutate(b = on)]
///    {
///        println!("B:");
///        const_print::<permutate!(CONST_INT)>();
///    }
/// }
/// ```
/// 
/// Would expand to:
/// 
/// ```rust
/// # fn const_print<const VAL: u32>() {
/// #     println!("{VAL:}");
/// # }
///
/// fn bar__on__off__CONST_INT_4() {
///    println!("A:");
///    const_print::<4>();
/// }
/// 
/// fn bar__off__on__CONST_INT_8() {
///     println!("B:");
///     const_print::<8>();
/// }
/// ```
/// 
/// In addition, permutations can be specified by file path, or an environment variable containing a file path:
/// 
/// ```rust
/// # use permutate_macro::permutate;
/// #[permutate(
///     parameters = {
///        foo: on | off,
///        bar: on | off
///    },
///    constants = {},
///    permutations = [
///        file("permutations.json", "path::to::this::module"),
///        env("SOME_ENV_VAR", "path::to::this::module")
///    ]
/// )]
/// fn func() {}
/// ```
/// 
/// The expected format of this file is as follows:
/// 
/// ```json
/// {
///  "path::to::this::module::func": [
///    {
///      "parameters": [
///        "on",
///        "on",
///      ],
///      "constants": {
///        "CONST_INT": 4,
///      }
///    },
///    {
///      "parameters": [
///        "off",
///        "on",
///      ],
///      "constants": {
///        "CONST_INT": 6,
///      }
///    },
///    {
///      "parameters": [
///        "off",
///        "off",
///      ],
///      "constants": {
///        "CONST_INT": 8,
///      }
///    }
///  ],
/// }
/// ```

#[proc_macro_attribute]
pub fn permutate(attr: TokenStream, item: TokenStream) -> TokenStream {
    permutate::macro_impl(parse_macro_input!(attr), parse_macro_input!(item))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
