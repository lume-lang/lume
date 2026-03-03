use lume_benchmark::*;

#[benchmark(
    args = [1_000, 10_000, 100_000, 1_000_000],
    counter = divan::counter::BytesCount::new(count * 16)
)]
fn g1_allocate(count: usize) -> lume_benchmark::Source {
    let capacity_tt = format!("{count}")
        .parse::<lume_benchmark::proc_macro2::Literal>()
        .unwrap();

    lume_benchmark::source! {
        struct Foo {
            pub a: Int32 = 8;
            pub b: Int32 = 16;
        }

        fn main() -> UInt64 {
            let i = 0;
            let arr = Array<Foo>::with_capacity( #capacity_tt );

            while i < #capacity_tt {
                arr.push(Foo { });
                i = i + 1;
            }

            arr.length
        }
    }
}
