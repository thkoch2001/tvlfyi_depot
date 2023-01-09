let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = [];
    RUSTUP_TOOLCHAIN = "nightly-2020-06-21";
  }
