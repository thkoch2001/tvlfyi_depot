{ depot, pkgs, python3, python3Lib }:

let
  transitiveLib = python3Lib {
    name = "transitive";
  } ''
    def transitive(s):
      return s + " 1 2 3"
  '';

  testLib = python3Lib {
    name = "test_lib";
    libraries = _: [ transitiveLib ];
  } ''
    import transitive
    def test():
      return transitive.transitive("test")
  '';

  pythonWithLib = python3 "python-with-lib" {
    libraries = _: [ testLib ];
  } ''
    import test_lib

    assert(test_lib.test() == "test 1 2 3")
  '';

in {
  inherit
    pythonWithLib;
}
