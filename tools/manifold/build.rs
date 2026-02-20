#![allow(clippy::disallowed_macros)]

fn main() {
    println!("cargo::rerun-if-changed=../../tests/");
}
