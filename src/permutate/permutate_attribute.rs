use proc_macro::Span;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Comma,
    Error,
};

use super::{
    keywords,
    permutations::{Constants, Permutations},
    types::Types,
    Parameters,
};

pub enum PermutateArgument {
    Parameters(Parameters),
    Constants(Constants),
    Types(Types),
    Permutations(Permutations),
}

impl Parse for PermutateArgument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(keywords::parameters) {
            Ok(PermutateArgument::Parameters(Parameters::parse(input)?))
        } else if lookahead.peek(keywords::constants) {
            Ok(PermutateArgument::Constants(Constants::parse(input)?))
        } else if lookahead.peek(keywords::types) {
            Ok(PermutateArgument::Types(Types::parse(input)?))
        } else if lookahead.peek(keywords::permutations) {
            Ok(PermutateArgument::Permutations(Permutations::parse(input)?))
        } else {
            Err(lookahead.error())
        }
    }
}

pub struct PermutateAttribute {
    arguments: Punctuated<PermutateArgument, Comma>,
}

impl Parse for PermutateAttribute {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attr = PermutateAttribute {
            arguments: Punctuated::parse_separated_nonempty(input)?,
        };

        attr.permutations()?
            .validate(attr.parameters()?, attr.constants()?, attr.types()?)?;

        Ok(attr)
    }
}

impl PermutateAttribute {
    pub fn parameters(&self) -> Result<&Parameters, Error> {
        self.arguments
            .iter()
            .find_map(|arg| {
                if let PermutateArgument::Parameters(parameters) = arg {
                    Some(parameters)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing parameters"))
    }

    pub fn constants(&self) -> Result<&Constants, Error> {
        self.arguments
            .iter()
            .find_map(|arg| {
                if let PermutateArgument::Constants(constants) = arg {
                    Some(constants)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing constants"))
    }

    pub fn types(&self) -> Result<&Types, Error> {
        self.arguments
            .iter()
            .find_map(|arg| {
                if let PermutateArgument::Types(types) = arg {
                    Some(types)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing types"))
    }

    pub fn permutations(&self) -> Result<&Permutations, Error> {
        self.arguments
            .iter()
            .find_map(|param| {
                if let PermutateArgument::Permutations(permutations) = param {
                    Some(permutations)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new(Span::call_site().into(), "Missing permutations"))
    }
}
