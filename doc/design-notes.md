# Design Notes

## Sexpr Format

### Indices & Type Uses

Unlike WAT, wabt-clj's Sexpr syntax does not support numeric indices wherever
symbolic ids are allowed. Only funcs and locals permit referencing by index.

In describing `typeuse` forms, the spec requires a sort of hash-cons scheme
for implicitly defined types. It says that "the smallest existing type index
whose definition in the current module is the function type" is used as the
associated index. It also says that "If no such index exists, then a new type
definition [...] is inserted at the end of the module."

This design forces an additional AST pass (WABT calls it `ResolveFuncTypes`)
to find these types and append their definitions. Since wabt-clj disallows
non-symbolic references, it's safe to _insert_ these definitions immediately
upon discovery that they are needed. If numeric references were allowed, this
would cause subsequent indices to need to be shifted. But since they are not,
the extra AST pass is avoided.
