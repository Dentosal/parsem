#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::{Diagnostic, Level};
use proc_macro2::{self, Ident, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AttrStyle, Attribute, DataEnum, DataStruct, DeriveInput,
    Fields, Type,
};

use std::sync::atomic::{AtomicU64, Ordering};

static UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
struct MatcherTarget {
    /// None on structs
    variant_name: Option<Ident>,
    /// None on unnamed fields, i.e. tuple structs
    field_name: Option<Ident>,
    /// None on unit structs
    field_type: Option<Type>,
}
impl MatcherTarget {
    fn span(&self) -> Option<Span> {
        [
            self.variant_name.clone().map(|v| v.span()),
            self.field_name.clone().map(|v| v.span()),
            self.field_type.clone().map(|v| v.span()),
        ]
        .iter()
        .filter_map(|x| x.clone())
        .reduce(|a, b| a.join(b).expect("MatcherTarget parts from different files"))
    }

    /// Extracts field, but only if there is exactly one field
    fn from_fields(variant_name: Option<Ident>, fields: &Fields) -> Self {
        let field_iter = match fields {
            Fields::Unnamed(f) => f.unnamed.iter(),
            Fields::Named(f) => f.named.iter(),
            Fields::Unit => {
                // Unit struct
                return Self {
                    variant_name,
                    field_name: None,
                    field_type: None,
                };
            }
        };

        let mut fs: Vec<_> = field_iter.collect();
        if fs.len() != 1 {
            Diagnostic::spanned(
                fields.span().unwrap(),
                Level::Error,
                "Field count incorrect, only zero or one fields are supported",
            )
            .emit();
            panic!("Disallowed field type");
        }
        let f = fs.pop().unwrap();
        Self {
            variant_name,
            field_name: f.ident.clone(),
            field_type: Some(f.ty.clone()),
        }
    }

    fn construct(&self, value: Option<TokenStream>) -> TokenStream {
        if value.is_some() {
            assert!(self.field_name.is_none(), "Field name without value");
        }

        if self.field_type.is_none() {
            assert!(value.is_none(), "Unit field doesn't take values");
        }

        if let Some(variant_name) = &self.variant_name {
            if let Some(value) = value {
                if let Some(field) = &self.field_name {
                    quote! { Self::#variant_name { #field: #value } }
                } else {
                    quote! { Self::#variant_name(#value) }
                }
            } else {
                quote! { Self::#variant_name }
            }
        } else {
            if let Some(value) = value {
                if let Some(f_name) = &self.field_name {
                    quote! { Self { #f_name: #value } }
                } else {
                    quote! { Self(#value) }
                }
            } else {
                quote! { Self {} }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ScanMode {
    /// Never scan anything to this
    Never,
    Fixed(Literal),
    Regex(Literal),
    RegexCapture(Literal),
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
                    "regex_capture" => ScanMode::RegexCapture(value),
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

    /// Variant is used to specify enum variant, None for structs
    fn generate_matcher(&self, target: &MatcherTarget) -> TokenStream {
        match self {
            ScanMode::Never => quote! {None},
            ScanMode::Regex(regex) => {
                let id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
                let re_ident = Ident::new(&format!("__PARSEM_RE_{}", id), Span::call_site());
                let value = target.construct(if target.field_type.is_some() {
                    Some(quote! { src[..length].to_owned() })
                } else {
                    None
                });
                quote! {
                    lazy_static::lazy_static! {
                        static ref #re_ident: Regex = Regex::new(&format!("^{}", #regex)).unwrap();
                    }
                    if let Some(m) = #re_ident.find(src) {
                        let length = m.end();
                        return Some((#value, length));
                    }
                }
            }
            ScanMode::RegexCapture(regex) => {
                let id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
                let re_ident = Ident::new(&format!("__PARSEM_RE_{}", id), Span::call_site());
                let value = target.construct(if target.field_type.is_some() {
                    Some(quote! { text })
                } else {
                    None
                });
                quote! {
                    lazy_static::lazy_static! {
                        static ref #re_ident: Regex = Regex::new(&format!("^{}", #regex)).unwrap();
                    }
                    if let Some(m) = #re_ident.captures(src) {
                        let length = m.get(0).unwrap().end();
                        let text = m.get(1).expect("No match found").as_str().to_owned();
                        return Some((#value, length));
                    }
                }
            }
            ScanMode::Fixed(fixed) => {
                let value = target.construct(if target.field_type.is_some() {
                    Some(quote! { #fixed })
                } else {
                    None
                });
                quote! {
                    if src.starts_with(#fixed) {
                        let length = #fixed.len();
                        return Some((#value, length));
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
        syn::Data::Struct(DataStruct { fields, .. }) => {
            let mut opts: Vec<_> = attrs.iter().filter_map(ScanMode::from_attr).collect();
            if opts.len() == 0 {
                panic!("Missing a top level scan attribute");
            } else if opts.len() > 1 {
                panic!("Only one scan attribute is accepted");
            }

            let target = MatcherTarget::from_fields(None, &fields);
            let matcher = opts.pop().unwrap().generate_matcher(&target);

            let output = quote! {
                impl ::parsem::Scannable for #ident {
                    fn scan(src: &str) -> Option<(Self, usize)> {
                        {#matcher}
                    }
                }
            };

            output.into()
        }
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let mut opts_per_variant = Vec::new();

            for variant in variants {
                let ident = variant.ident;
                let target = MatcherTarget::from_fields(Some(ident), &variant.fields);

                // Default scan mode
                let mut opts = ScanMode::Never;

                // See if override is specified
                for attr in variant.attrs {
                    if let Some(m) = ScanMode::from_attr(&attr) {
                        opts = m;
                        break;
                    }
                }

                opts_per_variant.push((target, opts));
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
        _ => panic!("enum or struct required"),
    }
}

#[proc_macro_derive(Node)]
pub fn macro_node(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    (quote! {}).into()
}
