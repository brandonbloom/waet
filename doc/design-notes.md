# Design Notes

## Sexpr Format

### Indices & Type Uses

Unlike WAT, wasm-clj's Sexpr syntax does not support numeric indices. Symbolic
ids are required everwhere an index is expected.

With respect to `typeuse` forms, the spec describes a sort of hash-cons scheme
for types without an accompanying index. It says that "the smallest existing
type index whose definition in the current module is the function type" is
used as the associated index. It also says that "If no such index exists, then
a new type definition [...] is inserted at the end of the module."

This design forces an additional AST pass (WABT calls it `ResolveFuncTypes`)
to find these types and append their definitions. Since wasm-clj disallows
non-symbolic references, it's safe to _insert_ these definitions immediately
upon discovery that they are needed. If numeric references were allowed, this
would cause subsequent indices to need to be shifted. But since they are not,
the extra AST pass is avoided.
