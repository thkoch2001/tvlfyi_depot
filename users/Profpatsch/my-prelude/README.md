# My Haskell Prelude

Contains various modules I’ve found useful when writing Haskell.

## Contents

A short overview:

### `MyPrelude.hs`

A collection of re-exports and extra functions. This does *not* replace the `Prelude` module from `base`, but rather should be imported *in addition* to `Prelude`.

Stuff like bad functions from prelude (partial stuff, or plain horrible stuff) are handled by a custom `.hlint` file, which you can find in [../.hlint.yaml]().

The common style of haskell they try to enable is what I call “left-to-right Haskell”,
where one mostly prefers forward-chaining operators like `&`/`<&>`/`>>=` to backwards operators like `$`/`<$>`/`<=<`. In addition, all transformation function should follow the scheme of `aToB` instead of `B.fromA`, e.g. `Text.unpack`/`Text.pack` -> `textToString`/`stringToText`. Includes a bunch of text conversion functions one needs all the time, in the same style.

These have been battle-tested in a production codebase of ~30k lines of Haskell.

### `Label.hs`

A very useful collection of anonymous labbeled tuples and enums of size 2 and 3. Assumes GHC >9.2 for `RecordDotSyntax` support.

### `Pretty.hs`

Colorful multiline pretty-printing of Haskell values.

### `Test.hs`

A wrapper around `hspec` which produces colorful test diffs.

### `Aeson.hs`

Helpers around Json parsing.

### `Data.Error.Tree`

Collect errors (from [`Data.Error`](https://hackage.haskell.org/package/error-1.0.0.0/docs/Data-Error.html)) into a tree, then display them in a nested fashion. Super useful for e.g. collecting and displaying nested parsing errors.

### `RunCommand.hs`

A module wrapping the process API with some helpful defaults for executing commands and printing what is executed to stderr.
