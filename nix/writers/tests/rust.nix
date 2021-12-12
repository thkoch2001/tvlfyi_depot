{ depot, pkgs, ... }:

let
  inherit (depot.nix.writers) rustSimple rustSimpleLib rustSimpleBin;

  inherit (pkgs) coreutils;

  run = drv:
    depot.nix.runExecline.local "run-${drv.name}" { } [
      "if"
      [ drv ]
      "importas"
      "out"
      "out"
      "${coreutils}/bin/touch"
      "$out"
    ];

  rustTransitiveLib = rustSimpleLib { name = "transitive"; } ''
    pub fn transitive(s: &str) -> String {
      let mut new = s.to_string();
      new.push_str(" 1 2 3");
      new
    }

    #[cfg(test)]
    mod tests {
      use super::*;

      #[test]
      fn test_transitive() {
        assert_eq!(transitive("foo").as_str(), "foo 1 2 3")
      }
    }
  '';

  rustTestLib = rustSimpleLib {
    name = "test_lib";
    dependencies = [ rustTransitiveLib ];
  } ''
    extern crate transitive;
    use transitive::{transitive};
    pub fn test() -> String {
      transitive("test")
    }
  '';

  rustWithLib = run (rustSimple {
    name = "rust-with-lib";
    dependencies = [ rustTestLib ];
  } ''
    extern crate test_lib;

    fn main() {
      assert_eq!(test_lib::test(), String::from("test 1 2 3"));
    }
  '');

in depot.nix.readTree.drvTargets { inherit rustTransitiveLib rustWithLib; }
