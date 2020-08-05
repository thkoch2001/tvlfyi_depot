# Transform QWERTY

Apply a series of transforms to a QWERTY keyboard.

## Usage

To run the program, enter the following:

```shell
$ runhaskell Main.hs --help
Usage: Main.hs (-t|--transforms ARG)
  Transform a QWERTY keyboard using a string of commands

Available options:
  -t,--transforms ARG      String of transforms where (e.g. "HHVS12VHVHS3")
  -h,--help                Show this help text
```

For example:

```shell
$ runhaskell Main.hs --transforms=HHVS12VHVHS3
[N][M][,][.][/][Z][X][C][V][B]
[H][J][K][L][;][A][S][D][F][G]
[Y][U][I][O][P][Q][W][E][R][T]
[6][7][8][9][0][1][2][3][4][5]
```

## Environment

You'll need `runhaskell`, so call `nix-shell` from this project's root directory.
