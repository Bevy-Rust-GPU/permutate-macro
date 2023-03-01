//! Procedural macro for permutating functions at compile time.

#![feature(proc_macro_span)]

extern crate proc_macro;

mod permutate;

use proc_macro::TokenStream;

use syn::parse_macro_input;

/// Generates conditional permutations of a function based on a supplied set of parameters.
///
/// For example:
///
/// ```
/// #[permutate(
///     parameters = {
///         a: on | off,
///         b: on | off,
///         c: on | off
///     },
///     permutations = [
///         (on, on, on),
///         (off, on, off),
///         (off, off, off)
///     ]
/// )]
/// fn foo() {
///     #[permutate(a = on)]
///     println!("A");
///
///     #[permutate(b = on)]
///     println!("B");
///
///     #[permutate(c = on)]
///     println!("C");
/// }
/// ```
///
/// Would expand to...
///
/// ```
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
/// fn foo__off__off__off() {
/// }
/// ```
/// 
/// In addition, permutations can be specified by file path,
/// or an environment variable containing a file path:
/// ```
/// #[permutate(
///     parameters = {
///         foo: on | off,
///         bar: on | off
///     },
///     permutations = [
///         file("permutations.json", "path::to::this::module"),
///         env("SOME_ENV_VAR", "path::to::this::module")
///     ],
/// )]
/// fn func() {}
/// ```
///
/// The expected format of this file is as follows:
///
/// ```
/// {
///     "path::to::this::module::func": [
///         ["on", "on"],
///         ["off", "on"],
///         ["off", "off"],
///     ]
/// }
/// ```
#[proc_macro_attribute]
pub fn permutate(attr: TokenStream, item: TokenStream) -> TokenStream {
    permutate::macro_impl(parse_macro_input!(attr), parse_macro_input!(item))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
