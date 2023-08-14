---
title: ".drvPath inconsistencies"
author:
 - tazjin
 - flokli
email:
 - tazjin@tvl.su
 - flokli@flokli.de
lang: en-GB
---

# Why .drvPath differs between Nix and Tvix

Nix and Tvix currently use a different approach when it comes to tracking string
contexts. Nix is using string contexts, whereas Tvix is doing reference scanning
[^1].

There are some real-life cases, for example during nixpkgs  bootstrapping, where
multiple different fixed-output derivations are  written to produce the same
hash.

For example, bootstrap sources that are downloaded early are fetched  using
a special "builder hack", in which the `builder` field of the  derivation is
populated with the magic string `builtins:fetchurl` and  the builder itself will
perform a fetch, with everything looking like a  normal derivation to the user.

These bootstrap sources are later on defined *again*, once `curl` is  available,
to be downloaded using the standard pkgs.fetchtarball  mechanism, but yielding
the *same* outputs (as the same files are being  fetched).

In our reference scanning implementation, this output scanning of FOD  will
yield whatever the *first* derivation was that produced the given  path as the
drv to be stored in the `inputDrvs` field of the derivation.


[^1]: https://inbox.tvl.su/depot/20230316120039.j4fkp3puzrtbjcpi@tp/T/#t
