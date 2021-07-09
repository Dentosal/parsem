#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::{Diagnostic, Level};
use proc_macro2::{self, Ident, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, AttrStyle, Attribute, DataEnum, DataStruct, DeriveInput, Fields, Type,
};

use std::sync::atomic::{AtomicU64, Ordering};

static UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
enum ScanMode {
    /// Use the wrapped type
    Inner {
        /// Variant is used to specify enum variant, None for structs
        variant: Option<Ident>,
        field: FieldSpecifier,
    },
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

    fn generate_matcher(&self) -> TokenStream {
        match self {
            ScanMode::Inner { variant, field } => {
                let t = field.type_.clone();
                if let Some(v) = variant {
                    if let Some(f_name) = field.name.clone() {
                        quote! {
                            Self::#v { #f_name:  #t::scan() }
                        }
                    } else {
                        quote! {
                            Self::#v(#t::scan())
                        }
                    }
                } else {
                    if let Some(f_name) = field.name.clone() {
                        quote! {
                            Self { #f_name:  #t::scan() }
                        }
                    } else {
                        quote! {
                            Self(#t::scan())
                        }
                    }
                }
            }
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
                        Some((src[..length].to_owned(), length))
                    } else {
                        None
                    }
                }
            }
            ScanMode::RegexCapture(regex) => {
                let id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
                let re_ident = Ident::new(&format!("__PARSEM_RE_{}", id), Span::call_site());
                quote! {
                    lazy_static::lazy_static! {
                        static ref #re_ident: Regex = Regex::new(&format!("^{}", #regex)).unwrap();
                    }
                    if let Some(m) = #re_ident.captures(src) {
                        let length = m.get(0).unwrap().end();
                        let text = m.get(1).expect("No match found").as_str().to_owned();
                        Some((text, length))
                    } else {
                        None
                    }
                }
            }
            ScanMode::Fixed(fixed) => quote! {
                if src.starts_with(#fixed) {
                    let length = #fixed.len();
                    Some((src[..length].to_owned(), length))
                } else {
                    None
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
struct FieldSpecifier {
    name: Option<Ident>,
    type_: Type,
}
impl FieldSpecifier {
    /// Extracts field, but only if there is exactly one field
    fn extract_single(fields: &Fields) -> Option<Self> {
        let field_iter = match fields {
            Fields::Unnamed(f) => f.unnamed.iter(),
            Fields::Named(f) => f.named.iter(),
            Fields::Unit => return None,
        };

        let mut fs: Vec<_> = field_iter.collect();
        if fs.len() != 1 {
            return None;
        }
        let f = fs.pop().unwrap();
        Some(Self {
            name: f.ident.clone(),
            type_: f.ty.clone(),
        })
    }
}

#[proc_macro_derive(Scan, attributes(scan))]
pub fn macro_scan(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input);

    match data {
        syn::Data::Struct(DataStruct { .. }) => {
            let mut opts: Vec<_> = attrs.iter().filter_map(ScanMode::from_attr).collect();
            if opts.len() == 0 {
                panic!("Missing a top level scan attribute");
            } else if opts.len() > 1 {
                panic!("Only one scan attribute is accepted");
            }

            let matcher = opts.pop().unwrap().generate_matcher();

            let output = quote! {
                impl ::parsem::Scannable for #ident {
                    fn scan(src: &str) -> Option<(Self, usize)> {
                        let (text, length) = {#matcher}?;
                        Some((Self(text), length))
                    }
                }
            };

            output.into()
        }
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let mut opts_per_variant = Vec::new();

            for variant in variants {
                let ident = variant.ident;
                let field = FieldSpecifier::extract_single(&variant.fields);
                let mut opts = field
                    .map(|field| ScanMode::Inner {
                        variant: Some(ident.clone()),
                        field,
                    })
                    .unwrap_or(ScanMode::Never);

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
                .map(|(variant, opts)| {
                    let matcher = opts.generate_matcher();
                    quote! {
                        if let Some((text, length)) = {#matcher} {
                            return (Self::#variant(text), length);
                        }
                    }
                })
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
