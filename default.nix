# At the time of this writing, this configuration was taken from @tazjin's
# default.nix from his depot. I've added, changed, and removed that parts that I
# don't need, and this is what remains.
{ ... }@args:

with builtins;

let
  fix = f: let x = f x; in x;

  # Global configuration that all packages are called with.
  config = self: {
    inherit self;
    pkgs = import <nixpkgs> {};
    depot = import <depot> {};
    briefcase = import <briefcase> {};
  };

  readTree' = import <depot/nix/readTree> {};

  # TODO: Find a better way to expose entire monorepo without introducing
  # "infinite recursion".
  localPkgs = readTree: {
    nixos = readTree ./nixos;
    blog = readTree ./blog;
    lisp = readTree ./lisp;
    gopkgs = readTree ./gopkgs;
    monzo_ynab = readTree ./monzo_ynab;
    third_party = readTree ./third_party;
    tools = readTree ./tools;
  };
in fix(self: {
  config = config self;
}

# Add local packages as structured by readTree
// (localPkgs (readTree' (self.config // { inherit (self) lib; })))
)
