{ depot, lib, ... }:

let
  inherit (depot.nix.runTestsuite)
    runNintTestsuite
    it
    assertEq
    assertThrows
    assertDoesNotThrow
    ;

  inherit (depot.nix.utils)
    isDirectory
    isRegularFile
    isSymlink
    ;

  assertUtilsPred = msg: act: exp: [
    (assertDoesNotThrow "${msg} does not throw" act)
    (assertEq msg (builtins.tryEval act).value exp)
  ];

  pathPredicates = it "judges paths correctly" (lib.flatten [
    # isDirectory
    (assertUtilsPred "directory isDirectory"
      (isDirectory ./directory) true)
    (assertUtilsPred "symlink not isDirectory"
      (isDirectory ./symlink-directory) false)
    (assertUtilsPred "file not isDirectory"
      (isDirectory ./directory/file) false)
    # isRegularFile
    (assertUtilsPred "file isRegularFile"
      (isRegularFile ./directory/file) true)
    (assertUtilsPred "symlink not isRegularFile"
      (isRegularFile ./symlink-file) false)
    (assertUtilsPred "directory not isRegularFile"
      (isRegularFile ./directory) false)
    # isSymlink
    (assertUtilsPred "symlink to file isSymlink"
      (isSymlink ./symlink-file) true)
    (assertUtilsPred "symlink to directory isSymlink"
      (isSymlink ./symlink-directory) true)
    (assertUtilsPred "symlink to symlink isSymlink"
      (isSymlink ./symlink-symlink-file) true)
    (assertUtilsPred "symlink to missing file isSymlink"
      (isSymlink ./missing) true)
    (assertUtilsPred "directory not isSymlink"
      (isSymlink ./directory) false)
    (assertUtilsPred "file not isSymlink"
      (isSymlink ./directory/file) false)
    # missing files throw
    (assertThrows "isDirectory throws on missing file"
      (isDirectory ./does-not-exist))
    (assertThrows "isRegularFile throws on missing file"
      (isRegularFile ./does-not-exist))
    (assertThrows "isSymlink throws on missing file"
      (isSymlink ./does-not-exist))
  ]);
in

runNintTestsuite "nix.utils" [
  pathPredicates
]
