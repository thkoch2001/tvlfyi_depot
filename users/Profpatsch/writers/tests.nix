{ depot, pkgs, python3Lib }:

let
  testLib = python3Lib {
    name = "test_lib";
  } ''
    def test():
      return "test"
  '';

  pythonWithLib = pkgs.writers.writePython3 "python-with-lib" {
    libraries = [ testLib ];
  } ''
    import test_lib

    assert(test_lib.test() == "test")
  '';

in {
  inherit
    pythonWithLib;
}
