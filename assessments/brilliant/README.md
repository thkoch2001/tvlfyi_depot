# Transform QWERTY

Apply a series of transforms to a QWERTY keyboard then use the new keyboard to
re-type a passage of text.

## Environment

You will need [Nix][nix] to build this program on your machine. The good news is
that you won't need any Haskell-specific dependencies like `ghc`, `cabal`, or
`stack`: just Nix.

Once you have Nix installed, to build the program, run the following from this
project's top-level directory:

```shell
$ nix-build
```

This should output an executable named `transform-keyboard` within a `result`
directory:

```shell
$ tree result
result
└── transform-keyboard
```

### Testing

To run the test suite, run the following from the project's top-level directory:

```shell
$ nix-shell
$ runhaskell Spec.hs
```

[nix]: https://nixos.org/download.html

## Usage

Here are some `--help` and usage examples:

```shell
$ ./result/transform-keyboard --help
Usage: transform-keyboard (-t|--transforms ARG) (-p|--passage ARG)
  Transform a QWERTY keyboard using a string of commands

Available options:
  -t,--transforms ARG      String of transforms where (e.g. "HHVS12VHVHS3")
  -p,--passage ARG         Input text to re-type
  -h,--help                Show this help text
```

Now a working example:

```shell
$ ./result/transform-keyboard --transforms=HHVS12VHVHS3 --passage='Hello,Brilliant.'
Typing: "Hello,Brilliant."
On this keyboard:
[H][J][K][L][;][Q][W][E][R][T]
[Y][U][I][O][P][1][2][3][4][5]
[6][7][8][9][0][Z][X][C][V][B]
[N][M][,][.][/][A][S][D][F][G]
Result: ZIVV4D/O3VV36APF
```

...and an example with an erroneous input (i.e. `!`):

```shell
$ ./result/transform-keyboard --transforms=HHVS12VHVHS3 --passage='Hello,Brilliant!'
Typing: "Hello,Brilliant!"
On this keyboard:
[H][J][K][L][;][Q][W][E][R][T]
[Y][U][I][O][P][1][2][3][4][5]
[6][7][8][9][0][Z][X][C][V][B]
[N][M][,][.][/][A][S][D][F][G]
Looks like at least one of the characters in your input passage doesn't fit on our QWERTY keyboard:
[1][2][3][4][5][6][7][8][9][0]
[Q][W][E][R][T][Y][U][I][O][P]
[A][S][D][F][G][H][J][K][L][;]
[Z][X][C][V][B][N][M][,][.][/]
```
