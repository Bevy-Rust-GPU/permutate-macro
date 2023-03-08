use std::collections::BTreeMap;

use json::JsonValue;
use proc_macro2::Span;
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Comma, Or, Paren, Star, Brace, Colon},
    Error, Ident, LitStr, braced, ExprLit, 
};

use super::{keywords, parameters::Parameters};

pub enum PermutationField {
    Parameters(Punctuated<PermutationVariant, Comma>),
    Constants(Punctuated<ConstantField, Comma>),
}

impl Parse for PermutationField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keywords::parameters) {
            let _ident: Ident = input.parse()?;
            let _eq: syn::token::Eq = input.parse()?;
            let content: ParseBuffer;
            let _bracket = bracketed!(content in input);
            Ok(PermutationField::Parameters(Punctuated::parse_separated_nonempty(&content)?))
        } else if lookahead.peek(keywords::constants) {
            let _ident: Ident = input.parse()?;
            let _eq: syn::token::Eq = input.parse()?;
            let content: ParseBuffer;
            let _brace = braced!(content in input);
            Ok(PermutationField::Constants(Punctuated::parse_terminated(&content)?))
        } else {
            Err(input.error("Unrecognized key"))
        }
    }
}

impl PermutationField {
    fn validate(&self, parameters: &Parameters, constants: &Constants) -> Result<(), Error> {
        match self {
            PermutationField::Parameters(params) => {
                for (i, param) in params.iter().enumerate() {
                    param.validate(parameters, i)?;
                }
            },
            PermutationField::Constants(cs) => {
                for constant in cs {
                    constant.validate(constants)?;
                }
            },
        }

        Ok(())
    }
}

pub struct ConstantField {
    key: Ident,
    _eq: syn::token::Eq,
    value: ExprLit,
}

impl Parse for ConstantField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(ConstantField {
            key: input.parse()?,
            _eq: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl ConstantField {
    fn validate(&self, constants: &Constants) -> Result<(), Error> {
        for constant in constants.constants.iter() {
            let key = match constant {
                ConstantsVariant::Bool(key) |
                ConstantsVariant::Uint(key) |
                ConstantsVariant::Int(key) => key,
            };

            if *key != self.key {
                continue;
            }

            match (constant, &self.value.lit) {
                (ConstantsVariant::Bool(_), syn::Lit::Bool(_)) |
                (ConstantsVariant::Uint(_), syn::Lit::Int(_)) |
                (ConstantsVariant::Int(_), syn::Lit::Int(_))  => (),
                _ => return Err(Error::new(self.value.lit.span(), "Mismatched constant type"))
            }
        }

        Ok(())
    }
}

pub struct SynPermutation {
    pub _brace: Brace,
    pub fields: Punctuated<PermutationField, Comma>,
}

impl Parse for SynPermutation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(SynPermutation {
            _brace: braced!(content in input),
            fields: Punctuated::parse_separated_nonempty(&content)?,
        })
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Permutation {
    pub parameters: Vec<Ident>,
    pub constants: BTreeMap<Ident, ConstantVal>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstantVal {
    Bool(bool),
    Uint(u32),
    Int(i32),
}

impl ToString for ConstantVal {
    fn to_string(&self) -> String {
        match self {
            ConstantVal::Bool(value) => value.to_string(),
            ConstantVal::Uint(value) => value.to_string(),
            ConstantVal::Int(value) => value.to_string(),
        }
    }
}

fn parse_file(
    mod_path: &str,
    file_path: &str,
    fn_ident: &Ident,
) -> Vec<Permutation> {
    let file = match std::fs::read_to_string(file_path) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("WARNING: Failed to read permutations file at {file_path:}: {e:}");
            return Default::default();
        }
    };

    let json = match json::parse(&file) {
        Ok(json) => json,
        Err(e) => {
            panic!("Failed to parse permutations file: {e:}");
        }
    };

    let JsonValue::Object(object) = json else {
        panic!("Top-level permutations JSON is not an object");
    };

    let source_path = mod_path.to_string() + "::" + &fn_ident.to_string();
    let Some(entry_point) = object.get(&source_path) else {
        eprintln!("WARNING: No JSON entry point for source path {source_path:}");
        return Default::default()
    };

    let JsonValue::Array(array) = entry_point else {
        panic!("JSON entry point for source path {source_path:} is not an array");
    };

    let values = array
        .into_iter()
        .map(|value| {
            let JsonValue::Object(object) = value else {
                panic!("JSON permutation for entry point {entry_point:} is not an object");
            };

            let Some(parameters) = object.get("parameters") else {
                panic!("JSON permutation for entry point {entry_point:} has no parameters key");
            };

            let JsonValue::Array(parameters) = parameters else {
                panic!("Parameters key is not an array");
            };

            let Some(constants) = object.get("constants") else {
                panic!("JSON permutation for entry point {entry_point:} has no constants key");
            };

            let JsonValue::Object(constants) = constants else {
                panic!("Constants key is not an object");
            };

            let parameters = parameters
                .into_iter()
                .map(|value| {
                    let Some(string) = value.as_str() else {
                        panic!("JSON permutation variant for entry point {entry_point:} is not a string");
                    };
                    Ident::new(string, Span::call_site())
                })
                .collect();

            let constants = constants.iter().map(|(key, value)| {
                let ident = Ident::new(key.into(), Span::call_site());

                if let Some(value) = value.as_bool() {
                    (ident, ConstantVal::Bool(value))
                }
                else if let Some(value) = value.as_u32() {

                    (ident, ConstantVal::Uint(value))
                }
                else if let Some(value) = value.as_i32() {

                    (ident, ConstantVal::Int(value))
                }
                else {
                    panic!("Unrecognized constant value type")
                }
            }).collect();

            Permutation {
                parameters, 
                constants,
            }
        })
        .collect::<Vec<_>>();

    values
}

impl SynPermutation {
    /// Ensure that all variants are defined in the provided [`Parameters`]
    fn validate(&self, parameters: &Parameters, constants: &Constants) -> Result<(), Error> {
        for field in self.fields.iter() {
            field.validate(parameters, constants)?;
        }

        Ok(())
    }

    pub fn parameters(&self) -> Result<&Punctuated<PermutationVariant, Comma>, Error> {
        self.fields
            .iter()
            .find_map(|arg| {
                if let PermutationField::Parameters(parameters) = arg {
                    Some(parameters)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing parameters"))
    }

    pub fn constants(&self) -> Result<&Punctuated<ConstantField, Comma>, Error> {
        self.fields
            .iter()
            .find_map(|arg| {
                if let PermutationField::Constants(constants) = arg {
                    Some(constants)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing constants"))
    }

    fn into_permutations(&self, parameters: &Parameters, constants: &Constants) -> Vec<Permutation> {
        let params = self.parameters().unwrap();

        let mut idents = vec![];
        for (i, variant) in params.iter().enumerate() {
            let mut ids = vec![];
            match variant {
                PermutationVariant::Explicit(explicit) => ids.extend(explicit.iter().cloned()),
                PermutationVariant::Glob(_) => {
                    ids.extend(parameters.parameters[i].variants.clone())
                }
            }
            idents.push(ids);
        }

        let consts = self.constants().unwrap();

        permutations(idents).into_iter().map(|permutation| Permutation {
            parameters: permutation,
            constants: constants.constants.iter().map(|constant| {
                let ident = match constant {
                    ConstantsVariant::Bool(ident) |
                    ConstantsVariant::Uint(ident) |
                    ConstantsVariant::Int(ident) => ident,
                };

                let candidate = consts.iter().find(|constant| constant.key == *ident).unwrap();

                match (constant, &candidate.value.lit) {
                    (ConstantsVariant::Bool(ident), syn::Lit::Bool(value)) => (ident.clone(), ConstantVal::Bool(value.value)),
                    (ConstantsVariant::Uint(_), syn::Lit::Int(value)) => (ident.clone(), ConstantVal::Uint(value.base10_parse::<u32>().unwrap())),
                    (ConstantsVariant::Int(_), syn::Lit::Int(value)) => (ident.clone(), ConstantVal::Int(value.base10_parse::<i32>().unwrap())),
                    _ => unreachable!() // Already validated
                }
            }).collect()
        }).collect()
    }
}

pub struct PermutationsFile {
    pub ident: Ident,
    pub paren: Paren,
    pub file: LitStr,
    pub comma: Comma,
    pub mod_path: LitStr,
}

impl Parse for PermutationsFile {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(PermutationsFile {
            ident: input.parse()?,
            paren: parenthesized!(content in input),
            file: content.parse()?,
            comma: content.parse()?,
            mod_path: content.parse()?,
        })
    }
}

pub struct PermutationsEnv {
    pub ident: Ident,
    pub paren: Paren,
    pub var: LitStr,
    pub comma: Comma,
    pub mod_path: LitStr,
}

impl Parse for PermutationsEnv {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(PermutationsEnv {
            ident: input.parse()?,
            paren: parenthesized!(content in input),
            var: content.parse()?,
            comma: content.parse()?,
            mod_path: content.parse()?,
        })
    }
}

pub enum PermutationsVariant {
    Literal(SynPermutation),
    File(PermutationsFile),
    Env(PermutationsEnv),
}

impl PermutationsVariant {
    fn validate(&self, parameters: &Parameters, constants: &Constants) -> Result<(), Error> {
        match self {
            PermutationsVariant::Literal(literal) => literal.validate(parameters, constants),
            PermutationsVariant::File(_) => {
                eprintln!("Warning: Validation is currently unsupported for permutation files.");
                Ok(())
            },
            PermutationsVariant::Env(_) => {
                eprintln!("Warning: Validation is currently unsupported for permutation environment vars.");
                Ok(())
            },
        }
    }

    fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters, constants: &Constants) -> Vec<Permutation> {
        match self {
            PermutationsVariant::Literal(literal) => literal.into_permutations(parameters, constants),
            PermutationsVariant::File(file) => {
                let mod_path = file.mod_path.value();
                let path = file.file.value();

                let mut file_path = Span::call_site().unwrap().source_file().path();
                file_path.pop();

                let path = file_path.join(path);
                let path = path.to_str().expect("Path is not valid unicode");

                parse_file(&mod_path, path, fn_ident)
            }
            PermutationsVariant::Env(env) => {
                let mod_path = env.mod_path.value();
                let var = env.var.value();

                let Ok(path) = std::env::var(var) else {
                    return Default::default();
                };

                parse_file(&mod_path, &path, fn_ident)
            }
        }
    }
}

impl Parse for PermutationsVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keywords::env) {
            Ok(PermutationsVariant::Env(input.parse()?))
        } else if lookahead.peek(keywords::file) {
            Ok(PermutationsVariant::File(input.parse()?))
        } else {
            Ok(PermutationsVariant::Literal(input.parse()?))
        }
    }
}

pub struct Constants {
    pub _ident: Ident,
    pub _eq: syn::token::Eq,
    pub _brace: Brace,
    pub constants: Punctuated<ConstantsVariant, Comma>,
}

impl Parse for Constants {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(Constants {
            _ident: input.parse()?,
            _eq: input.parse()?,
            _brace: braced!(content in input),
            constants: Punctuated::parse_terminated(&content)?,
        })
    }
}

pub enum ConstantsVariant {
    Bool(Ident),
    Uint(Ident),
    Int(Ident),
}

impl Parse for ConstantsVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let _colon: Colon = input.parse()?;

        let lookahead = input.lookahead1();
        if lookahead.peek(super::keywords::bool) {
            let _tok: Ident = input.parse()?;
            Ok(ConstantsVariant::Bool(ident))
        }
        else if lookahead.peek(super::keywords::u32) {
            let _tok: Ident = input.parse()?;
            Ok(ConstantsVariant::Uint(ident))
        }
        else if lookahead.peek(super::keywords::i32) {
            let _tok: Ident = input.parse()?;
            Ok(ConstantsVariant::Int(ident))
        }
        else {
            Err(input.error("Unrecognized token"))
        }
    }
}

pub struct Permutations {
    pub _ident: Ident,
    pub _eq: syn::token::Eq,
    pub _bracket: Bracket,
    pub permutations: Punctuated<PermutationsVariant, Comma>,
}

impl Parse for Permutations {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(Permutations {
            _ident: input.parse()?,
            _eq: input.parse()?,
            _bracket: bracketed!(content in input),
            permutations: Punctuated::parse_separated_nonempty(&content)?,
        })
    }
}

impl Permutations {
    /// Ensure that all variants are defined in the provided [`Parameters`]
    pub fn validate(&self, parameters: &Parameters, constants: &Constants) -> Result<(), Error> {
        for permutation in self.permutations.iter() {
            permutation.validate(parameters, constants)?;
        }

        Ok(())
    }

    pub fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters, constants: &Constants) -> Vec<Permutation> {
        let mut permutations = vec![];
        for permutation in self.permutations.iter() {
            permutations.extend(permutation.into_permutations(fn_ident, parameters, constants))
        }
        permutations.sort();
        permutations.dedup();
        permutations
    }

    pub fn file_paths(&self) -> Vec<String> {
        self.permutations
            .iter()
            .flat_map(|permutation| match permutation {
                PermutationsVariant::File(file) => {
                    let path = file.file.value();

                    let mut file_path = Span::call_site().unwrap().source_file().path();
                    file_path.pop();

                    let path = file_path.join(path);
                    let path = path.to_str().expect("Path is not valid unicode");

                    if std::fs::File::open(path).is_ok() {
                        Some(file.file.value())
                    } else {
                        None
                    }
                }
                PermutationsVariant::Env(env) => {
                    let env = env.var.value();

                    let Ok(path) = std::env::var(env) else {
                        return None;
                    };

                    if std::fs::File::open(&path).is_ok() {
                        Some(path)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }

    pub fn env_vars(&self) -> Vec<String> {
        self.permutations
            .iter()
            .flat_map(|permutation| match permutation {
                PermutationsVariant::Env(env) => {
                    let env = env.var.value();

                    std::env::var(env).ok()
                }
                _ => None,
            })
            .collect()
    }
}

pub enum PermutationVariant {
    Explicit(Punctuated<Ident, Or>),
    Glob(Star),
}

impl Parse for PermutationVariant {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Star) {
            Ok(PermutationVariant::Glob(Star::parse(input)?))
        } else {
            Ok(PermutationVariant::Explicit(
                Punctuated::<Ident, Or>::parse_separated_nonempty(input)?,
            ))
        }
    }
}

impl PermutationVariant {
    fn validate(&self, parameters: &Parameters, index: usize) -> Result<(), Error> {
        match self {
            PermutationVariant::Explicit(idents) => {
                for ident in idents {
                    let parameter = &parameters.parameters[index];
                    if !parameter
                        .variants
                        .iter()
                        .find(|candidate| *candidate == ident)
                        .is_some()
                    {
                        let valid_variants = parameter
                            .variants
                            .iter()
                            .map(ToString::to_string)
                            .enumerate()
                            .map(|(i, variant)| {
                                variant
                                    + if i < parameter.variants.len() - 1 {
                                        ", "
                                    } else {
                                        ""
                                    }
                            })
                            .collect::<String>();

                        return Err(Error::new(
                            ident.span(),
                            format!(
                                "Invalid variant {ident:}. Valid variants are [{valid_variants:}].",
                            ),
                        ));
                    }
                }
            }
            _ => (),
        }

        Ok(())
    }
}

/// Convert a list of lists of variants into a list of those variants' permutations
fn permutations<T: Clone>(sets: Vec<Vec<T>>) -> Vec<Vec<T>> {
    permutations_inner(sets.into_iter(), Default::default())
}

fn permutations_inner<It: Clone + Iterator<Item = Vec<T>>, T: Clone>(
    mut sets: It,
    acc: Vec<T>,
) -> Vec<Vec<T>> {
    sets.next()
        .map(|set| {
            set.into_iter()
                .flat_map(|item| {
                    let mut tmp = acc.clone();
                    tmp.push(item);
                    permutations_inner(sets.clone(), tmp)
                })
                .collect()
        })
        .unwrap_or_else(|| vec![acc])
}
