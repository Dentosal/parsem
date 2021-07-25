use std::fmt;

use log;

pub use parsem_macros::*;

pub trait Scannable
where
    Self: Sized,
{
    fn scan(src: &str) -> Option<(Self, usize)>;
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub location: Option<Location>,
    parent: Option<Box<ParseError>>,
}
impl ParseError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            location: None,
            parent: None,
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = Some(location);
        self
    }

    pub fn with_parent(mut self, parent: ParseError) -> Self {
        self.parent = Some(Box::new(parent));
        self
    }

    pub fn depth(&self) -> usize {
        let mut cursor = self.clone();
        let mut result = 0;
        while let Some(parent) = cursor.parent {
            cursor = *parent;
            result += 1;
        }
        result
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n [", self.message)?;
        if let Some(loc) = &self.location {
            write!(f, "in {} offset {}", loc.file, loc.offset)?;
        } else {
            write!(f, "location unknown")?;
        }
        write!(f, "]")?;

        if let Some(parent) = &self.parent {
            write!(f, "\n{}", parent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ParseResult<T> {
    /// Parsed value, if successful
    pub parsed: Option<T>,
    /// Token count parsed, consumed iff parsed is Some
    pub token_count: usize,
    /// If parsed is None, this is the error that stopped parsing
    /// If parsing was succesful, then this is the last inner
    pub error: Option<ParseError>,
}
impl<T> ParseResult<T> {
    pub fn empty() -> Self {
        Self {
            parsed: None,
            token_count: 0,
            error: None,
        }
    }

    pub fn new(parsed: T, token_count: usize) -> Self {
        Self {
            parsed: Some(parsed),
            token_count,
            error: None,
        }
    }

    pub fn without_value<R>(self) -> ParseResult<R> {
        ParseResult {
            parsed: None,
            token_count: self.token_count,
            error: self.error,
        }
    }

    pub fn error(error: ParseError) -> Self {
        ParseResult {
            parsed: None,
            token_count: 0,
            error: Some(error),
        }
    }

    pub fn add_err(self, error: ParseError) -> Self {
        Self {
            parsed: self.parsed,
            token_count: self.token_count,
            error: Some(if let Some(err) = self.error {
                error.with_parent(err)
            } else {
                error
            }),
        }
    }

    pub fn map<R, F: FnMut(T) -> R>(self, f: F) -> ParseResult<R> {
        ParseResult {
            parsed: self.parsed.map(f),
            token_count: self.token_count,
            error: self.error,
        }
    }
}

pub trait Parsable<TT: Eq>
where
    Self: Sized + std::fmt::Debug,
{
    fn match_parse(tokens: &[Token<TT>]) -> ParseResult<Self>;
    fn tokens(&self) -> Vec<Token<TT>>;
}

/// Boxed field
impl<TT: Eq, T: Parsable<TT>> Parsable<TT> for Box<T> {
    fn match_parse(tokens: &[Token<TT>]) -> ParseResult<Self> {
        T::match_parse(&tokens).map(Box::new)
    }

    fn tokens(&self) -> Vec<Token<TT>> {
        (*self).tokens()
    }
}

/// Optional field
impl<TT: Eq, T: Parsable<TT>> Parsable<TT> for Option<T> {
    fn match_parse(tokens: &[Token<TT>]) -> ParseResult<Self> {
        ::log::trace!("Matching optional field");
        let result = T::match_parse(&tokens);
        if let Some(value) = result.parsed {
            ::log::trace!("Matched optional field");
            ParseResult {
                parsed: Some(Some(value)),
                token_count: result.token_count,
                error: result.error,
            }
        } else {
            ::log::trace!("No match for optional field");
            ParseResult {
                parsed: Some(None),
                token_count: 0,
                error: result.error,
            }
        }
    }

    fn tokens(&self) -> Vec<Token<TT>> {
        self.as_ref().map(|v| v.tokens()).unwrap_or(Vec::new())
    }
}

/// Non-empty sequence
impl<TT: Eq, T: Parsable<TT>> Parsable<TT> for Vec<T> {
    fn match_parse(tokens: &[Token<TT>]) -> ParseResult<Self> {
        let mut result = Vec::new();
        let mut index = 0;
        let mut item_error = None;
        while index < tokens.len() {
            ::log::trace!(
                "Matching vec i={}, t={:?}",
                index,
                tokens[..index]
                    .iter()
                    .map(|t| t.text.clone())
                    .collect::<Vec<_>>()
            );
            let item = T::match_parse(&tokens[index..]);
            if let Some(err) = item.error {
                item_error = Some(err);
            }
            if let Some(value) = item.parsed {
                // repeated empty field?
                // TODO: better error for this
                assert_ne!(item.token_count, 0);
                result.push(value);
                index += item.token_count;
            } else {
                ::log::trace!(
                    "No match for vec item {} {:?}",
                    std::any::type_name::<T>(),
                    item_error.as_ref().unwrap()
                );
                break;
            }
        }

        if index == tokens.len() {
            if let Some(e) = item_error {
                let err = ParseError::new("Repetition encountered EOF".to_owned());
                return ParseResult::error(err.with_parent(e));
            }
        }

        if result.is_empty() {
            let mut err = ParseError::new(format!(
                "Empty repetition of {}",
                std::any::type_name::<T>()
            ))
            .with_location(tokens[0].location.clone());
            if let Some(e) = item_error {
                err = err.with_parent(e);
            }
            let mut r = ParseResult::error(err);
            r.token_count = index;
            r
        } else {
            println!("Vec done");
            ::log::trace!(
                "Matched vec i={}, t={:?}, c={:?}",
                index,
                tokens[..index]
                    .iter()
                    .map(|t| t.text.clone())
                    .collect::<Vec<_>>(),
                result.len()
            );
            ParseResult {
                parsed: Some(result),
                token_count: index,
                error: item_error,
            }
        }
    }

    fn tokens(&self) -> Vec<Token<TT>> {
        let mut result = Vec::new();
        for item in self {
            result.extend(item.tokens());
        }
        result
    }
}

macro_rules! tuple_impls {
    ( $( $name:ident $index:tt ),+ ) => {
        impl<TT: Eq, $($name: Parsable<TT>),+> Parsable<TT> for ($($name,)+)
        {
           fn match_parse(tokens: &[Token<TT>]) -> $crate::ParseResult<Self> {
                let mut index = 0;
                ::log::trace!(
                    "Matching tuple of {:?}",
                    ($(stringify!($name),)*)
                );
                let fields = ($({
                    ::log::trace!(
                        "Matching tuple item {:?} i={} t={:?}",
                        stringify!($name),
                        index,
                        tokens[..index]
                            .iter()
                            .map(|t| t.text.clone())
                            .collect::<Vec<_>>()
                    );
                    let parsed = $name::match_parse(&tokens[index..]);
                    if let Some(value) = parsed.parsed {
                        index += parsed.token_count;
                        value
                    } else {
                        return parsed.without_value().add_err(
                            $crate::ParseError::new(format!(
                                "Tuple field {} {} did not match",
                                stringify!($name),
                                ::std::any::type_name::<$name>(),
                            ))
                        );
                    }
                },)+);
                ::log::trace!(
                    "Matched tuple of {:?} i={} t={:?}",
                    ($(stringify!($name),)*),
                    index,
                    tokens[..index]
                        .iter()
                        .map(|t| t.text.clone())
                        .collect::<Vec<_>>()
                );
                $crate::ParseResult {
                    parsed: Some(fields),
                    token_count: index,
                    error: None,
                }
            }
            fn tokens(&self) -> Vec<Token<TT>> {
                let mut result = Vec::new();
                $( result.extend(self.$index.tokens()); )*
                result
            }
        }
    };
}

tuple_impls!(T0 0);
tuple_impls!(T0 0, T1 1);
tuple_impls!(T0 0, T1 1, T2 2);
tuple_impls!(T0 0, T1 1, T2 2, T3 3);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub file: String,
    pub offset: usize,
    pub length: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<T: Eq> {
    pub location: Location,
    pub type_: T,
    pub text: String,
}

#[macro_export]
macro_rules! token_wrapper {
    ($ttype: ident; $($name: ident),+ $(,)?) => {$(
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Node)]
        #[token_type($ttype)]
        #[single($name)]
        pub struct $name {
            pub token: ::parsem::Token<$ttype>,
        }
        // impl $name {
        //     pub fn tokens(&self) -> Vec<::parsem::Token<$ttype>> {
        //         vec![self.token.clone()]
        //     }
        // }
    )*};
}
