use std::hint::black_box;
use std::path::PathBuf;

use criterion::*;
use lume_bench::{compile, run};

fn young_gen(c: &mut Criterion) {
    let root = PathBuf::from(std::env!("CARGO_MANIFEST_DIR")).join("benches");

    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);

    let mut group = c.benchmark_group("alloc");
    group.plot_config(plot_config);

    group.bench_function(BenchmarkId::new("G1", 100), |b| {
        #[allow(unused_variables, reason = "bug with `black_box`")]
        let bin_path = compile(&root.join("gc_young_alloc_100.lm")).unwrap();

        b.iter(|| run(black_box(&bin_path)).unwrap())
    });

    group.bench_function(BenchmarkId::new("G1", 1000), |b| {
        #[allow(unused_variables, reason = "bug with `black_box`")]
        let bin_path = compile(&root.join("gc_young_alloc_1k.lm")).unwrap();

        b.iter(|| run(black_box(&bin_path)).unwrap())
    });

    group.bench_function(BenchmarkId::new("G1", 10_000), |b| {
        #[allow(unused_variables, reason = "bug with `black_box`")]
        let bin_path = compile(&root.join("gc_young_alloc_10k.lm")).unwrap();

        b.iter(|| run(black_box(&bin_path)).unwrap())
    });

    group.bench_function(BenchmarkId::new("G1", 100_000), |b| {
        #[allow(unused_variables, reason = "bug with `black_box`")]
        let bin_path = compile(&root.join("gc_young_alloc_100k.lm")).unwrap();

        b.iter(|| run(black_box(&bin_path)).unwrap())
    });

    group.finish();
}

criterion_group!(benches, young_gen);
criterion_main!(benches);
