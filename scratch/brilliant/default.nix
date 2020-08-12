let
  briefcase = import /home/wpcarro/briefcase {};
in briefcase.buildHaskell.program {
  name = "transform-keyboard";
  srcs = builtins.path {
    path = ./.;
    name = "transform-keyboard-src";
  };
  deps = hpkgs: with hpkgs; [
    optparse-applicative
    unordered-containers
    split
    rio
  ];
  ghcExtensions = [];
}
