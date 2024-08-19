{ depot, lib, ... }:

let
  gitignoreNix = import depot.third_party.sources."gitignore.nix" { inherit lib; };
in
{
  __functor = _: gitignoreNix.gitignoreSource;

  # expose extra functions here
  inherit (gitignoreNix)
    gitignoreFilter
    ;
}
