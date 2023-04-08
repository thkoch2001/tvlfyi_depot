# haskell-module-deps

An executable that when run in a project directory containing `.hs` files in `./src` will output a png/graph of how those modules import each other, transitively.

Useful for getting an overview, finding weird import edges, figuring out how to get more compilation parallelism into your Haskell project.
