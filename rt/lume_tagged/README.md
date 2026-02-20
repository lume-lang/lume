The `lume_tagged` crates provides a way to tag pointers with extra information, such as whether they are heap-allocated objects, or whether they have any drop method attached. Since all object pointers are aligned to the pointer-width, we have 3 bits (or 2 bits for 32-bit architectures) available for tagging.

This crate is mostly used by `lume_gc` and `lume_codegen`.
