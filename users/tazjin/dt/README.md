dt
==

It's got a purpose.

## Usage:

```
nix-build -E '(import (builtins.fetchGit "https://git.tazj.in/") {}).fun.dt'
./result/bin/dt --one ... --two ...
```
