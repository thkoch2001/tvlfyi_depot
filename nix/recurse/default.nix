{ depot, lib, ... }:
# Functions handling recursion.
#
# Mostly inspired by https://hackage.haskell.org/package/data-fix-0.2.1/docs/Data-Fix.html#g:1
let

  Attr = {
    Functor = {
      fmap = f: lib.mapAttrs (_: f);
    };
  };

  # Transform a non-recursive fold function `alg` into a recursive fold.
  # cata
  #   :: { fmap :: f a -> f b }
  #   -> (f a -> a)
  #   -> (Fix f -> a)
  #
  # where
  #   Fix f = f (Fix f)
  #   and fmap/f form a Functor
  cata = d@{ fmap }: alg: v:
    alg (fmap (cata d alg) v);

  # Transform a non-recursive generator funtion `coalg` into a recursive unfold.
  # ana
  #   :: { fmap :: f a -> f b }
  #   -> (a -> f a)
  #   -> (a -> Fix f)
  #
  # where
  #   Fix f = f (Fix f)
  #   and fmap/f form a Functor
  ana = d@{ fmap }: coalg: v:
    fmap (ana d coalg) (coalg v);

  # Fuse a fold and an unfold into a value transformer.
  # hylo
  #   :: { fmap :: f a -> f b }
  #   -> (f b -> b)
  #   -> (a -> f a)
  #   -> (a -> b)
  #
  # where
  #   fmap/f form a Functor
  hylo = d@{ fmap }: alg: coalg: v:
    cata d alg (ana d coalg v);


  tests = import ./tests.nix {
    inherit
      depot
      lib
      cata
      hylo
      ;
  };

in {
   inherit
    cata
    ana
    hylo
    tests
    ;
}
