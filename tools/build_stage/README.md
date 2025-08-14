# `build_stage`

`build_stage` is a helper crate used by other tools in the `tools/` subdirectory. It is meant
to streamline building Lume source code into each distinct compiler artifact. Namely, `build_stage`
can build Lume source code into:
- HIR via `build_hir`
- type inferred context via `type_inference`
- type checked context via `type_check`
- TIR via `build_tir`
- MIR via `build_mir`
