{ depot, pkgs, python3, python3Lib, rustSimpleLib, rustSimple }:

let
  run = drv: depot.nix.runExecline.local "run-${drv.name}" {} [
    "if" [ drv ]
    "importas" "out" "out"
    "${pkgs.coreutils}/bin/touch" "$out"
  ];

  pythonTransitiveLib = python3Lib {
    name = "transitive";
  } ''
    def transitive(s):
      return s + " 1 2 3"
  '';

  pythonTestLib = python3Lib {
    name = "test_lib";
    libraries = _: [ pythonTransitiveLib ];
  } ''
    import transitive
    def test():
      return transitive.transitive("test")
  '';

  pythonWithLib = run (python3 {
    name = "python-with-lib";
    libraries = _: [ pythonTestLib ];
  } ''
    import test_lib

    assert(test_lib.test() == "test 1 2 3")
  '');


  rustTransitiveLib = rustSimpleLib {
    name = "transitive";
  } ''
    pub fn transitive(s: &str) -> String {
      let mut new = s.to_string();
      new.push_str(" 1 2 3");
      new
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


in {
  inherit
    pythonWithLib
    rustTransitiveLib
    rustWithLib
    ;
}
