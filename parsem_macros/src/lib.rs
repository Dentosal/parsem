#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::{Diagnostic, Level};
use proc_macro2::{self, Ident, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, token::Token, AttrStyle, Attribute, DataEnum, DataStruct,
    DeriveInput, Fields, Path, Type, TypePath,
};

use std::sync::atomic::{AtomicU64, Ordering};

static UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
enum ScanMode {
    /// Never scan anything to this
    Never,
    Fixed(Literal),
    Regex(Literal),
}
impl ScanMode {
    fn from_attr(attr: &Attribute) -> Option<Self> {
        if attr.style == AttrStyle::Outer && attr.path.is_ident("scan") {
            let mut t = attr.tokens.clone().into_iter();
            if let Some(TokenTree::Group(group)) = t.next() {
                let mut stream: Vec<_> = group.stream().into_iter().collect();
                if stream.len() != 2 {
                    Diagnostic::spanned(
                        group.span().unwrap(),
                        Level::Error,
                        "Argument count incorrect",
                    )
                    .emit();
                    panic!("Expected exactly two arguments");
                }
                let b = stream.pop().unwrap();
                let a = stream.pop().unwrap();

                let a_span = a.span().unwrap();
                let b_span = a.span().unwrap();

                let type_ = if let TokenTree::Ident(t) = a {
                    t
                } else {
                    Diagnostic::spanned(
                        a_span,
                        Level::Error,
                        "This should be specify matcher type",
                    )
                    .emit();
                    panic!("Expected matcher type")
                };

                let value = if let TokenTree::Literal(lit) = b {
                    lit
                } else {
                    Diagnostic::spanned(b_span, Level::Error, "This should be a string literal")
                        .emit();
                    panic!("Expected literal")
                };

                Some(match type_.to_string().as_str() {
                    "never" => ScanMode::Never,
                    "fixed" => ScanMode::Fixed(value),
                    "regex" => ScanMode::Regex(value),
                    _ => {
                        Diagnostic::spanned(a_span, Level::Error, "No such matcher").emit();
                        panic!("Unknown matcher type");
                    }
                })
            } else {
                unreachable!("No group in attribute")
            }
        } else {
            None
        }
    }

    /// Variant is used to specify enum variant
    fn generate_matcher(&self, variant: &Ident) -> TokenStream {
        match self {
            ScanMode::Never => quote! {None},
            ScanMode::Regex(regex) => {
                let id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
                let re_ident = Ident::new(&format!("__PARSEM_RE_{}", id), Span::call_site());
                quote! {
                    lazy_static::lazy_static! {
                        static ref #re_ident: Regex = Regex::new(&format!("^{}", #regex)).unwrap();
                    }
                    if let Some(m) = #re_ident.find(src) {
                        let length = m.end();
                        return Some((Self::#variant, length));
                    }
                }
            }
            ScanMode::Fixed(fixed) => {
                quote! {
                    if src.starts_with(#fixed) {
                        let length = #fixed.len();
                        return Some((Self::#variant, length));
                    }
                }
            }
        }
    }
}

#[proc_macro_derive(Scan, attributes(scan))]
pub fn macro_scan(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input);

    match data {
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let mut opts_per_variant = Vec::new();

            for variant in variants {
                let ident = variant.ident;

                // Default scan mode
                let mut opts = ScanMode::Never;

                // See if override is specified
                for attr in variant.attrs {
                    if let Some(m) = ScanMode::from_attr(&attr) {
                        opts = m;
                        break;
                    }
                }

                opts_per_variant.push((ident, opts));
            }

            let alternatives: Vec<_> = opts_per_variant
                .iter()
                .map(|(target, opts)| opts.generate_matcher(target))
                .collect();

            let output = quote! {
                impl ::parsem::Scannable for #ident {
                    fn scan(src: &str) -> Option<(Self, usize)> {
                        #( #alternatives )*
                        None
                    }
                }
            };

            output.into()
        }
        _ => panic!("enum required"),
    }
}

fn attr_token_type(attr: &Attribute) -> Option<TokenStream> {
    if attr.style == AttrStyle::Outer && attr.path.is_ident("token_type") {
        let mut t = attr.tokens.clone().into_iter();
        if let Some(TokenTree::Group(group)) = t.next() {
            return Some(group.stream());
        }
    }
    None
}

fn attr_wraps_variant(attr: &Attribute) -> Option<TokenStream> {
    if attr.style == AttrStyle::Outer && attr.path.is_ident("single") {
        let mut t = attr.tokens.clone().into_iter();
        if let Some(TokenTree::Group(group)) = t.next() {
            return Some(group.stream());
        }
    }
    None
}

fn assert_only_field_token(fields: &Fields) {
    if fields.len() == 0 {
        panic!("No fields specified, but at least `token` must be present");
    }

    if fields.len() > 1 {
        panic!("Exactly one field, `token`, must be present");
    }

    for f in fields {
        if let Some(field_name) = &f.ident {
            if field_name.to_string() == "token" {
                return;
            }
        }
    }

    panic!("Field `token` not found");
}

#[proc_macro_derive(Node, attributes(token_type, single))]
pub fn macro_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident: root_ident,
        data,
        attrs,
        generics,
        ..
    } = parse_macro_input!(input);

    let wraps_variant = attrs
        .iter()
        .filter_map(|attr| attr_wraps_variant(attr))
        .nth(0);

    let token_type_type = match attrs.iter().filter_map(|attr| attr_token_type(attr)).nth(0) {
        Some(tt) => tt,
        None => panic!("Should have #[toke_type(TypeName)] attribute"),
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    match data {
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let cases = variants.iter().map(|v| {
                let vname = v.ident.clone();
                if let Fields::Unnamed(fs) = &v.fields {
                    let fs: Vec<_> = fs.unnamed.iter().collect();
                    if fs.len() == 1 {
                        let f = fs[0];
                        let type_ = &f.ty;
                        return quote! {
                            ::log::trace!("Matching case {}::{} at {:?}", stringify!(#root_ident), stringify!(#vname), &tokens[0]);
                            let parsed = #type_::match_parse(&tokens[index..]);
                            if parsed.parsed.is_some() {
                                ::log::trace!("Matched case");
                                return parsed.map(Self::#vname);
                            } else if let Some(err) = parsed.error {
                                variant_errors.push((parsed.token_count, stringify!(#vname), err));
                            }
                        };
                    }
                }
                Diagnostic::spanned(
                    v.fields.span().unwrap(),
                    Level::Error,
                    "Variant should have only a single unnamed item",
                )
                .emit();
                panic!("Variant should have only a single unnamed item");
            });
            let vars = variants.iter().map(|v| v.ident.clone());
            let output = quote! {
                impl #impl_generics ::parsem::Parsable<#token_type_type> for #root_ident #ty_generics
                #where_clause {
                    fn match_parse(tokens: &[::parsem::Token<#token_type_type>])
                    -> ::parsem::ParseResult<Self> {
                        let mut index = 0;
                        let mut variant_errors = Vec::new();
                        #( #cases )*
                        ::log::trace!("No cases matched");
                        let mut ve = variant_errors
                            .iter()
                            .map(|(tc, a, b)| (tc, a, b.clone()))
                            .collect::<Vec<_>>();
                        ve.sort_by(|a, b| a.0.cmp(&b.0));

                        let mut res = ::parsem::ParseResult::empty();

                        if let Some((token_count, best_vname, best_error)) = ve.pop() {
                            res.token_count = *token_count;
                            res = res.add_err(best_error);
                            res = res.add_err(::parsem::ParseError::new(format!("Best guess: {}", best_vname)));
                        }

                        res.add_err(::parsem::ParseError::new("None of the variants matched".to_owned()))
                    }
                    fn tokens(&self) -> Vec<::parsem::Token<#token_type_type>> {
                        match self {
                            #( Self::#vars(f) => f.tokens(), )*
                        }
                    }
                }
            };
            output.into()
        }
        syn::Data::Struct(DataStruct { fields, .. }) => {
            if let Some(variant) = wraps_variant {
                if !generics.params.is_empty() {
                    Diagnostic::spanned(
                        generics.span().unwrap(),
                        Level::Error,
                        "No generic argument allowed for variant wrappers",
                    )
                    .emit();
                    panic!("Not able to derive Parseable for this type");
                }

                // This is a wrapped type
                assert_only_field_token(&fields);

                let output = quote! {
                    impl ::parsem::Parsable<#token_type_type> for #root_ident {
                        fn match_parse(tokens: &[::parsem::Token<#token_type_type>])
                        -> ::parsem::ParseResult<Self> {
                            if let Some(token) = tokens.first() {
                                if token.type_ == #token_type_type::#variant {
                                    return ::parsem::ParseResult::new(Self { token: token.clone() }, 1);
                                }
                                ::log::trace!("No match: expected {} got {:?}",
                                    stringify!(#variant), token.type_);


                                ::parsem::ParseResult::error(parsem::ParseError::new(
                                    format!("Expected {:?} but got {:?}", stringify!(#variant), token.type_)
                                ).with_location(token.location.clone()))
                            } else {
                                ::log::trace!("No match: expected {} got EOF",
                                    stringify!(#token_type_type::#variant));

                                ::parsem::ParseResult::error(parsem::ParseError::new(
                                    format!("Expected {:?} but encountered EOF", stringify!(#variant))
                                ))
                            }
                        }
                        fn tokens(&self) -> Vec<::parsem::Token<#token_type_type>> {
                            vec![self.token.clone()]
                        }
                    }
                };

                return output.into();
            } else {
                if fields.is_empty() {
                    panic!("No fields specified, but at least one must be present");
                }

                match fields {
                    Fields::Named(fs) => {
                        let matchers = fs.named.iter().map(|field| {
                            let ident = field.ident.as_ref().unwrap();
                            let type_ = &field.ty;

                            let option_heuristic = if let Type::Path(tp) = &field.ty {
                                tp.qself.is_none() &&
                                    tp.path.segments
                                    .last()
                                    .map(|s| s.ident.to_string() == "Option")
                                    .unwrap_or(false)
                            } else {
                                false
                            };

                            let include_opt = if option_heuristic {
                                quote! {
                                    if value.is_none() {
                                        optionals.push((stringify!(#ident), stringify!(#type_)));
                                    }
                                }
                            } else {quote! {}};

                            quote! {
                                let #ident = {
                                    ::log::trace!("Matching field {}.{} to {}",
                                        stringify!(#root_ident), stringify!(#ident), stringify!(#type_)
                                    );
                                    let parsed = <#type_>::match_parse(&tokens[index..]);
                                    if let Some(value) = parsed.parsed {
                                        #include_opt;
                                        ::log::trace!("Matched field {}.{}",
                                            stringify!(#root_ident), stringify!(#ident)
                                        );
                                        index += parsed.token_count;
                                        value
                                    } else {
                                        ::log::trace!("No match for field {}.{}",
                                            stringify!(#root_ident), stringify!(#ident)
                                        );
                                        let mut errmsg = format!("Field {}.{} did not match {}",
                                            stringify!(#root_ident), stringify!(#ident), stringify!(#type_)
                                        );

                                        if !optionals.is_empty() {
                                            errmsg.push_str(&format!(" or optionals {:?}", optionals));
                                        }

                                        return ::parsem::ParseResult {
                                            parsed: None,
                                            token_count: parsed.token_count + index,
                                            error: Some(::parsem::ParseError::new(errmsg)
                                                .with_location(tokens[index].location.clone())
                                                .with_parent(parsed.error.unwrap()))
                                        };
                                    }
                                };
                            }
                        });

                        let field_names: Vec<_> =
                            fs.named.iter().map(|f| f.ident.clone().unwrap()).collect();

                        let output = quote! {
                            impl #impl_generics ::parsem::Parsable<#token_type_type> for #root_ident #ty_generics
                            #where_clause {
                                fn match_parse(tokens: &[::parsem::Token<#token_type_type>])
                                -> ::parsem::ParseResult<Self> {
                                    ::log::trace!("Matching struct {}",
                                        stringify!(#root_ident)
                                    );
                                    // Keep track of previous optional fields to show possible continuations
                                    let mut optionals: Vec<(&'static str, &'static str)> = Vec::new();
                                    let mut index = 0;
                                    #( #matchers )*
                                    ::log::trace!("Matched struct {}", stringify!(#root_ident));
                                    ::parsem::ParseResult {
                                        parsed: Some(Self { #( #field_names ),* }, ),
                                        token_count: index,
                                        error: None // TODO
                                    }
                                }
                                fn tokens(&self) -> Vec<::parsem::Token<#token_type_type>> {
                                    let mut result = Vec::new();
                                    #( result.extend(self.#field_names.tokens()); )*
                                    result
                                }
                            }
                        };
                        return output.into();
                    }
                    Fields::Unnamed(_fs) => todo!("Unnamed fields"),
                    Fields::Unit => unreachable!(),
                }
            }
        }
        _ => panic!("struct or enum required"),
    }
}
