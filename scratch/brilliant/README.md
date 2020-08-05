# Transform QWERTY

Apply a series of transforms to a QWERTY keyboard then use the new keyboard to
re-type a passage of text.

## Usage

Here are some `--help` and usage examples:

```shell
$ runhaskell Main.hs --help
Usage: Main.hs (-t|--transforms ARG) (-p|--passage ARG)
  Transform a QWERTY keyboard using a string of commands

Available options:
  -t,--transforms ARG      String of transforms where (e.g. "HHVS12VHVHS3")
  -p,--passage ARG         Input text to re-type
  -h,--help                Show this help text
```

Now a working example:

```shell
$ runhaskell Main.hs --transforms=HHVS12VHVHS3 --passage='Hello,Brilliant.'
Typing: "Hello,Brilliant."
On this keyboard:
[N][M][,][.][/][Z][X][C][V][B]
[H][J][K][L][;][A][S][D][F][G]
[Y][U][I][O][P][Q][W][E][R][T]
[6][7][8][9][0][1][2][3][4][5]
Result: QKRRF30LDRRDY1;4
```

...and an example with an erroneous input (i.e. `!`):

```shell
$ runhaskell Main.hs --transforms=HHVS12VHVHS3 --passage='Hello,Brilliant!'
Typing: "Hello,Brilliant!"
On this keyboard:
[N][M][,][.][/][Z][X][C][V][B]
[H][J][K][L][;][A][S][D][F][G]
[Y][U][I][O][P][Q][W][E][R][T]
[6][7][8][9][0][1][2][3][4][5]
Looks like at least one of the characters in your input passage doesn't fit on our QWERTY keyboard:
[1][2][3][4][5][6][7][8][9][0]
[Q][W][E][R][T][Y][U][I][O][P]
[A][S][D][F][G][H][J][K][L][;]
[Z][X][C][V][B][N][M][,][.][/]
```

## Environment

You'll need `runhaskell` and a few other Haskell libraries, so call `nix-shell`
from this project's root directory.

## Testing

To run the test suite:

```shell
$ runhaskell Spec.hs
```
