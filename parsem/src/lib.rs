pub use parsem_macros::*;

pub trait Scannable
where
    Self: Sized,
{
    fn scan(src: &str) -> Option<(Self, usize)>;
}
