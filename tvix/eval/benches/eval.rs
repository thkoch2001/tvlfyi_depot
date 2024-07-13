use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;
#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn interpret(code: &str) {
    tvix_eval::Evaluation::builder_pure()
        .build()
        .evaluate(code, None);
}

fn eval_literals(c: &mut Criterion) {
    c.bench_function("int", |b| {
        b.iter(|| {
            interpret(black_box("42"));
        })
    });
}

fn eval_merge_attrs(c: &mut Criterion) {
    c.bench_function("merge small attrs", |b| {
        b.iter(|| {
            interpret(black_box("{ a = 1; b = 2; } // { c = 3; }"));
        })
    });

    c.bench_function("merge large attrs with small attrs", |b| {
        let large_attrs = format!(
            "{{{}}}",
            (0..10000).map(|n| format!("a{n} = {n};")).join(" ")
        );
        let expr = format!("{large_attrs} // {{ c = 3; }}");
        b.iter(move || {
            interpret(black_box(&expr));
        })
    });

    c.bench_function("merge large attrs with large attrs", |b| {
        let large_attrs1 = format!(
            "{{{}}}",
            (0..10000).map(|n| format!("a{n} = {n};")).join(" ")
        );
        let large_attrs2 = format!(
            "{{{}}}",
            (100000..50000).map(|n| format!("a{n} = {n};")).join(" ")
        );
        let expr = format!("{large_attrs1} // {large_attrs2}");
        b.iter(move || {
            interpret(black_box(&expr));
        })
    });
}

criterion_group!(benches, eval_literals, eval_merge_attrs);
criterion_main!(benches);
