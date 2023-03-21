pub mod attributes;
pub mod keywords;
pub mod parameter_conditional;
pub mod parameters;
pub mod permutate_attribute;
pub mod permutations;

use proc_macro::Span;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use syn::{
    parse::Parse, parse_quote, visit_mut::VisitMut, AngleBracketedGenericArguments, Block, Error,
    Expr, ExprCall, ExprLit, ExprMethodCall, GenericArgument, Ident, ItemFn,
    Lit, LitBool, LitInt, Signature,
};

use crate::permutate::permutations::Permutation;

use self::{
    attributes::Attributes, parameter_conditional::ParameterConditional, parameters::Parameters,
};

pub fn macro_impl(
    attr: permutate_attribute::PermutateAttribute,
    item_fn: ItemFn,
) -> Result<TokenStream, Error> {
    let parameters = attr.parameters()?;
    let constants = attr.constants()?;

    // Calculate permutations
    let permutations = attr.permutations()?;

    let file_paths = permutations.file_paths();
    let env_vars = permutations.env_vars();

    let permutations = permutations.into_permutations(&item_fn.sig.ident, parameters, constants);

    // Iterate permutations
    let mut permutation_fns = vec![];

    struct PermutationVisitor<'a> {
        parameters: &'a Parameters,
        permutation: &'a Permutation,
        ident: &'a Ident,
    }

    impl VisitMut for PermutationVisitor<'_> {
        fn visit_expr_method_call_mut(&mut self, expr_method_call: &mut ExprMethodCall) {
            if let Some(turbofish) = expr_method_call.turbofish.as_mut() {
                let mut args: Vec<GenericArgument> = vec![];

                for arg in turbofish.args.iter() {
                    match arg {
                        syn::GenericArgument::Type(ty) => match ty {
                            syn::Type::Macro(m) => {
                                if m.mac.path.get_ident().unwrap() == self.ident {
                                    let tokens = &m.mac.tokens;
                                    let ident: Ident = parse_quote!(#tokens);
                                    if let Some(val) = self.permutation.constants.get(&ident) {
                                        args.push(GenericArgument::Const(Expr::Lit(
                                            ExprLit {
                                                attrs: Default::default(),
                                                lit: match val {
                                                    permutations::ConstantVal::Bool(val) => {
                                                        Lit::Bool(LitBool {
                                                            value: *val,
                                                            span: ident.span(),
                                                        })
                                                    }
                                                    permutations::ConstantVal::Uint(val) => {
                                                        Lit::Int(LitInt::new(
                                                            &val.to_string(),
                                                            ident.span(),
                                                        ))
                                                    }
                                                    permutations::ConstantVal::Int(val) => {
                                                        Lit::Int(LitInt::new(
                                                            &val.to_string(),
                                                            ident.span(),
                                                        ))
                                                    }
                                                },
                                            },
                                        )))
                                    } else {
                                        args.push(arg.clone())
                                    }
                                }
                            }
                            _ => args.push(arg.clone()),
                        },
                        _ => args.push(arg.clone()),
                    }
                }

                turbofish.args = args.into_iter().collect();
            }

            syn::visit_mut::visit_expr_method_call_mut(self, expr_method_call)
        }

        fn visit_angle_bracketed_generic_arguments_mut(
            &mut self,
            angle_bracketed_generic_args: &mut AngleBracketedGenericArguments,
        ) {
            let mut args: Vec<GenericArgument> = vec![];

            for arg in angle_bracketed_generic_args.args.iter() {
                match arg {
                    GenericArgument::Type(ty) => match ty {
                        syn::Type::Macro(m) => {
                            if m.mac.path.get_ident().unwrap() == self.ident {
                                let tokens = &m.mac.tokens;
                                let ident: Ident = parse_quote!(#tokens);
                                if let Some(val) = self.permutation.constants.get(&ident) {
                                    args.push(GenericArgument::Const(Expr::Lit(ExprLit {
                                        attrs: Default::default(),
                                        lit: match val {
                                            permutations::ConstantVal::Bool(val) => {
                                                Lit::Bool(LitBool {
                                                    value: *val,
                                                    span: ident.span(),
                                                })
                                            }
                                            permutations::ConstantVal::Uint(val) => Lit::Int(
                                                LitInt::new(&val.to_string(), ident.span()),
                                            ),
                                            permutations::ConstantVal::Int(val) => Lit::Int(
                                                LitInt::new(&val.to_string(), ident.span()),
                                            ),
                                        },
                                    })))
                                } else {
                                    args.push(arg.clone())
                                }
                            }
                        }
                        _ => args.push(arg.clone()),
                    },
                    _ => args.push(arg.clone()),
                }
            }

            angle_bracketed_generic_args.args = args.into_iter().collect();

            syn::visit_mut::visit_angle_bracketed_generic_arguments_mut(
                self,
                angle_bracketed_generic_args,
            );
        }

        fn visit_signature_mut(&mut self, signature: &mut Signature) {
            let mut inputs = vec![];

            for item in signature.inputs.iter() {
                if let Some(item) = apply_parameter_conditional(
                    self.parameters,
                    self.permutation,
                    self.ident,
                    item.clone(),
                )
                .unwrap()
                {
                    inputs.push(item);
                }
            }

            signature.inputs = inputs.into_iter().collect();

            syn::visit_mut::visit_signature_mut(self, signature);
        }

        fn visit_block_mut(&mut self, block: &mut Block) {
            let mut stmts = vec![];

            for stmt in block.stmts.iter() {
                if let Some(item) = apply_parameter_conditional(
                    self.parameters,
                    self.permutation,
                    self.ident,
                    stmt.clone(),
                )
                .unwrap()
                {
                    stmts.push(item);
                }
            }

            block.stmts = stmts.into_iter().collect();

            syn::visit_mut::visit_block_mut(self, block);
        }

        fn visit_expr_call_mut(&mut self, expr_call: &mut ExprCall) {
            let mut args = vec![];

            for arg in expr_call.args.iter() {
                if let Some(arg) = apply_parameter_conditional(
                    self.parameters,
                    self.permutation,
                    self.ident,
                    arg.clone(),
                )
                .unwrap()
                {
                    args.push(arg);
                }
            }

            expr_call.args = args.into_iter().collect();

            syn::visit_mut::visit_expr_call_mut(self, expr_call);
        }
    }

    for permutation in permutations.into_iter() {
        // Create a new copy of the function
        let mut item_fn = item_fn.clone();

        // Generate name from permutation
        let ident = item_fn.sig.ident.to_string()
            + &permutation
                .parameters
                .iter()
                .map(ToString::to_string)
                .map(|t| "__".to_string() + &t)
                .collect::<String>()
            + &permutation
                .constants
                .iter()
                .map(|(key, value)| key.to_string() + "_" + &value.to_string())
                .map(|t| "__".to_string() + &t)
                .collect::<String>();

        let ident = Ident::new(&ident, item_fn.sig.ident.span());
        item_fn.sig.ident = ident.clone();

        // Macro ident for matching inner attributes
        let span = Span::call_site().into();
        let ident = Ident::new("permutate", span);

        // Walk syntax tree and apply attributes
        syn::visit_mut::visit_item_fn_mut(
            &mut PermutationVisitor {
                parameters,
                permutation: &permutation,
                ident: &ident,
            },
            &mut item_fn,
        );

        permutation_fns.push(item_fn);
    }

    Ok(quote! {
        #(const _: &'static str = include_str!(#file_paths);)*
        #(const _: Option<&'static str> = option_env!(#env_vars);)*
        #(#permutation_fns)*
    })
}

/// Returns a filter function that will parse the given attribute,
/// extract a parameter conditional from its arguments,
/// and return None if the conditional evaluates false
fn apply_parameter_conditional<'a, T>(
    parameters: &'a Parameters,
    permutation: &'a Permutation,
    attr_ident: &'a Ident,
    mut input: T,
) -> Result<Option<T>, Error>
where
    T: Parse + ToTokens,
{
    if let Ok(mut attrs) = syn::parse::<Attributes<T>>(quote!(#input).into()) {
        if let Some(attr) = attrs.remove(&attr_ident) {
            let parameter_conditional =
                attr.parse_args_with(ParameterConditional::parse(parameters))?;

            // Safe to unwrap, since conditional parameters have already been validated
            let idx = parameters
                .parameters
                .iter()
                .position(|parameter| parameter.ident == parameter_conditional.parameter)
                .unwrap();

            if permutation.parameters[idx] != parameter_conditional.variant {
                return Ok(None);
            };
        }

        input = parse_quote!(#attrs);
    }

    Ok(Some(input))
}
