{ depot, pkgs, ... }:

let
  inherit (depot.users.Profpatsch.writers)
    python3Lib
    python3
    ;

  inherit (pkgs)
    coreutils
    ;

  run = drv: depot.nix.runExecline.local "run-${drv.name}" { } [
    "if"
    [ drv ]
    "importas"
    "out"
    "out"
    "${coreutils}/bin/touch"
    "$out"
  ];

  pythonTransitiveLib = python3Lib
    {
      name = "transitive";
    } ''
    def transitive(s):
      return s + " 1 2 3"
  '';

  pythonTestLib = python3Lib
    {
      name = "test_lib";
      libraries = _: [ pythonTransitiveLib ];
    } ''
    import transitive
    def test():
      return transitive.transitive("test")
  '';

  pythonWithLib = run (python3
    {
      name = "python-with-lib";
      libraries = _: [ pythonTestLib ];
    } ''
    import test_lib

    assert(test_lib.test() == "test 1 2 3")
  '');

in
depot.nix.readTree.drvTargets {
  inherit
    pythonWithLib
    ;
}
