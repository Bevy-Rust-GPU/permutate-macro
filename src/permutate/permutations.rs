use std::collections::BTreeMap;

use json::JsonValue;
use proc_macro2::{Span, TokenStream};
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Comma, Or, Paren, Star, Brace, Colon},
    Error, Ident, LitStr, braced, ExprLit, parse_quote, TypePath, 
};

use crate::permutate::types::Type;

use super::{keywords, parameters::Parameters, types::Types};

pub enum PermutationField {
    Parameters(Punctuated<PermutationVariant, Comma>),
    Constants(Punctuated<ConstantField, Comma>),
    Types(Punctuated<TypeField, Comma>),
}

impl Parse for PermutationField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(keywords::parameters) {
            let _ident: Ident = input.parse()?;
            let _eq: syn::token::Eq = input.parse()?;
            let content: ParseBuffer;
            let _bracket = bracketed!(content in input);
            Ok(PermutationField::Parameters(Punctuated::parse_terminated(&content)?))
        } else if lookahead.peek(keywords::constants) {
            let _ident: Ident = input.parse()?;
            let _eq: syn::token::Eq = input.parse()?;
            let content: ParseBuffer;
            let _brace = braced!(content in input);
            Ok(PermutationField::Constants(Punctuated::parse_terminated(&content)?))
        } else if lookahead.peek(keywords::types) {
            let _ident: Ident = input.parse()?;
            let _eq: syn::token::Eq = input.parse()?;
            let content: ParseBuffer;
            let _brace = braced!(content in input);
            Ok(PermutationField::Types(Punctuated::parse_terminated(&content)?))
        }else {
            Err(input.error("Unrecognized key"))
        }
    }
}

impl PermutationField {
    fn validate(&self, parameters: &Parameters, constants: &Constants, types: &Types) -> Result<(), Error> {
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
            PermutationField::Types(ts) => {
                for ty in ts {
                    ty.validate(types)?;
                }
            }
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

pub struct TypeField {
    key: Ident,
    _eq: syn::token::Eq,
    value: TypePath,
}

impl Parse for TypeField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(TypeField {
            key: input.parse()?,
            _eq: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl TypeField {
    fn validate(&self, types: &Types) -> Result<(), Error> {
        if types.types.iter().find(|ty| ty.key == self.key).is_none() {
            let valid_types = types
                .types
                .iter()
                .map(|ty| ty.key.to_string())
                .enumerate()
                .map(|(i, variant)| {
                    variant
                        + if i < types.types.len() - 1 {
                            ", "
                        } else {
                            ""
                        }
                })
                .collect::<String>();

            return Err(Error::new(Span::call_site(), format!("Invalid type {}, valid types are [{valid_types:}]", self.key)));
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

#[derive(Default, Clone)]
pub struct Permutation {
    pub parameters: Vec<Ident>,
    pub constants: BTreeMap<Ident, ConstantVal>,
    pub types: BTreeMap<Ident, TypePath>,
}

impl PartialEq for Permutation {
    fn eq(&self, other: &Self) -> bool {
        self.parameters.eq(&other.parameters) && self.constants.eq(&other.constants) && self.types.keys().eq(other.types.keys())
    }
}

impl Eq for Permutation {}

impl PartialOrd for Permutation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        [self.parameters.partial_cmp(&other.parameters), self.constants.partial_cmp(&other.constants), self.types.keys().partial_cmp(other.types.keys())].into_iter().fold(None, |acc, next| {
            match (acc, next) {
                (None, None) => None,
                (None, Some(rhs)) => Some(rhs),
                (Some(lhs), None) => Some(lhs),
                (Some(lhs), Some(rhs)) => Some(lhs.then(rhs)),
            }
        })
    }
}

impl Ord for Permutation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.parameters.cmp(&other.parameters).then(self.constants.cmp(&other.constants)).then(self.types.keys().cmp(other.types.keys()))
    }
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

    let source_path = if mod_path.is_empty() {
        fn_ident.to_string()
    } else {
        mod_path.to_string() + "::" + &fn_ident.to_string()
    };

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

            let Some(types) = object.get("types") else {
                panic!("JSON permutation for entry point {entry_point:} has no types key");
            };

            let JsonValue::Object(types) = types else {
                panic!("Types key is not an object");
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

            let types = types.iter().map(|(key, value)| {
                let key = Ident::new(key.into(), Span::call_site());
                let Some(value) = value.as_str() else {
                    panic!("JSON permutation variant for entry point {entry_point:} is not a string");
                };
                eprintln!("Parsing expr {value:}");
                let tokens: TokenStream = value.parse().expect("Invalid tokens");
                let value: TypePath = parse_quote!(#tokens);
                (key, value)
            }).collect();

            Permutation {
                parameters, 
                constants,
                types,
            }
        })
        .collect::<Vec<_>>();

    values
}

impl SynPermutation {
    /// Ensure that all variants are defined in the provided [`Parameters`]
    fn validate(&self, parameters: &Parameters, constants: &Constants, types: &Types) -> Result<(), Error> {
        for field in self.fields.iter() {
            field.validate(parameters, constants, types)?;
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

    pub fn types(&self) -> Result<&Punctuated<TypeField, Comma>, Error> {
        self.fields
            .iter()
            .find_map(|arg| {
                if let PermutationField::Types(types) = arg {
                    Some(types)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing types"))
    }

    fn into_permutations(&self, parameters: &Parameters, constants: &Constants, types: &Types) -> Vec<Permutation> {
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
        let tys = self.types().unwrap();

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
            }).collect(),
            types: types.types.iter().map(|ty| {
                let ident = &ty.key;
                let candidate = tys.iter().find(|ty| ty.key == *ident).unwrap();

                (ty.key.clone(), candidate.value.clone())
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
    fn validate(&self, parameters: &Parameters, constants: &Constants, types: &Types) -> Result<(), Error> {
        match self {
            PermutationsVariant::Literal(literal) => literal.validate(parameters, constants, types),
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

    fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters, constants: &Constants, types: &Types) -> Vec<Permutation> {
        match self {
            PermutationsVariant::Literal(literal) => literal.into_permutations(parameters, constants, types),
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
    pub fn validate(&self, parameters: &Parameters, constants: &Constants, types: &Types) -> Result<(), Error> {
        for permutation in self.permutations.iter() {
            permutation.validate(parameters, constants, types)?;
        }

        Ok(())
    }

    pub fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters, constants: &Constants, types: &Types) -> Vec<Permutation> {
        let mut permutations = vec![];
        for permutation in self.permutations.iter() {
            permutations.extend(permutation.into_permutations(fn_ident, parameters, constants, types))
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
