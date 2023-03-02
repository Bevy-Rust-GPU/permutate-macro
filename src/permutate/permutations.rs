use json::JsonValue;
use proc_macro2::Span;
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Comma, Or, Paren, Star},
    Error, Ident, LitStr,
};

use super::{keywords, parameters::Parameters};

pub struct Permutation {
    pub _paren: Paren,
    pub variants: Punctuated<PermutationVariant, Comma>,
}

impl Parse for Permutation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content: ParseBuffer;
        Ok(Permutation {
            _paren: parenthesized!(content in input),
            variants: Punctuated::parse_separated_nonempty(&content)?,
        })
    }
}

fn parse_file(mod_path: &str, file_path: &str, fn_ident: &Ident) -> Vec<Vec<Ident>> {
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
            let JsonValue::Array(array) = value else {
                panic!("JSON permutation for entry point {entry_point:} is not an array");
            };

            array
                .into_iter()
                .map(|value| {
                    let Some(string) = value.as_str() else {
                        panic!("JSON permutation variant for entry point {entry_point:} is not a string");
                    };
                    Ident::new(string, Span::call_site())
                })
                .collect()
        })
        .collect::<Vec<Vec<_>>>();

    values
}

impl Permutation {
    /// Ensure that all variants are defined in the provided [`Parameters`]
    fn validate(&self, parameters: &Parameters) -> Result<(), Error> {
        for (i, variant) in self.variants.iter().enumerate() {
            variant.validate(parameters, i)?;
        }

        Ok(())
    }

    fn into_permutations(&self, parameters: &Parameters) -> Vec<Vec<Ident>> {
        let mut idents = vec![];
        for (i, variant) in self.variants.iter().enumerate() {
            let mut ids = vec![];
            match variant {
                PermutationVariant::Explicit(explicit) => ids.extend(explicit.iter().cloned()),
                PermutationVariant::Glob(_) => {
                    ids.extend(parameters.parameters[i].variants.clone())
                }
            }
            idents.push(ids);
        }

        permutations(idents)
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
    Literal(Permutation),
    File(PermutationsFile),
    Env(PermutationsEnv),
}

impl PermutationsVariant {
    fn validate(&self, parameters: &Parameters) -> Result<(), Error> {
        match self {
            PermutationsVariant::Literal(literal) => literal.validate(parameters),
            PermutationsVariant::File(_) => Ok(()),
            PermutationsVariant::Env(_) => Ok(()),
        }
    }

    fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters) -> Vec<Vec<Ident>> {
        match self {
            PermutationsVariant::Literal(literal) => literal.into_permutations(parameters),
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
    pub fn validate(&self, parameters: &Parameters) -> Result<(), Error> {
        for permutation in self.permutations.iter() {
            permutation.validate(parameters)?;
        }

        Ok(())
    }

    pub fn into_permutations(&self, fn_ident: &Ident, parameters: &Parameters) -> Vec<Vec<Ident>> {
        let mut permutations = vec![];
        for permutation in self.permutations.iter() {
            permutations.extend(permutation.into_permutations(fn_ident, parameters))
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
                    let mod_path = file.mod_path.value();
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
                    let mod_path = env.mod_path.value();
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
                    let mod_path = env.mod_path.value();
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