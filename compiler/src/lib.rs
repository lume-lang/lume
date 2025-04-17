#[macro_export]
macro_rules! bug {
    ($($args:expr),*) => (
        panic!("{}", ::std::format_args!( $($args),* ))
    );
}

pub mod driver;
pub mod hir;
pub mod id;
mod std;
pub mod thir;
pub mod typech;
