# At the time of this writing, this configuration was taken from @tazjin's
# default.nix from his depot. I've added, changed, and removed that parts that I
# don't need, and this is what remains.
{ ... }@args:

with builtins;

let
  fix = f: let x = f x; in x;

  # Global configuration that all packages are called with.
  config = pkgs: {
    inherit pkgs;
  };

  readTree' = import /home/wpcarro/depot/nix/readTree {};

  # TODO: Find a better way to expose entire monorepo without introducing
  # "infinite recursion".
  localPkgs = readTree: {
    blog = readTree ./blog;
    third_party = readTree ./third_party;
  };
in fix(self: {
  config = config self;

  # Expose readTree for downstream repo consumers.
  readTree = {
    __functor = x: (readTree' x.config);
    config = self.config;
  };
}

# Add local packages as structured by readTree
// (localPkgs (readTree' (self.config // { inherit (self) lib; })))
)
