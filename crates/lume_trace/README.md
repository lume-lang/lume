# `lume_trace`

This crate exists as a conditional layer between Lume and [`tracing`](https://docs.rs/tracing/latest/tracing/index.html).
It attempts to prevent the performance impact of using tracing macros throughout the compiler, even when not enabled.
It should be noted that the `tracing` crate is still a non-optional dependency of this crate, meaning it will always
be pulled and compiled.

By default, tracing is not enabled. To enable it, use the `enable` feature when declaring the crate as a dependency:
```toml
[dependencies]
lume_trace = { path = "../../crates/lume_trace" }

[features]
tracing = ["lume_trace/enable"]
```
