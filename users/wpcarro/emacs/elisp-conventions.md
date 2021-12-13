# Elisp Conventions

Some of this aligns with existing style guides. Some of it does not.

In general, prefer functions with fixed arities instead of variadic
alternatives.

- Namespace functions with `namespace/function-name`
- Use `ensure`, `assert`, `refute` whenever possible.
- When talking about encoding and decoding, let's use the words "encoding" and
  "decoding" rather than the myriad of other variants that appear like:
  - `marshalling` and `unmarshalling`
  - `parse` and `deparse`, `serialize`, `stringify`
  - `unpickle` and `pickle` (Python)
  - `from-string` and `to-string`
  - TODO: Add more examples of these; there should be close to a dozen.
- Annotate assertions with `!` endings.
- Prefer the Scheme style of `predicate?`
- Variadic functions *should* encode this by appending * onto their
  name. E.g. `maybe/nil?*`
