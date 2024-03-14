{
  # list of package attribute names to get at run time
  packageNamesAtRuntimeJsonPath,
}:
let
  pkgs = import <nixpkgs> { };

  getPkg = pkgName: pkgs.${pkgName};

  packageNamesAtRuntime = builtins.fromJSON (builtins.readFile packageNamesAtRuntimeJsonPath);

  runtime = map getPkg packageNamesAtRuntime;
in
pkgs.symlinkJoin {
  name = "symlink-farm";
  paths = runtime;
}
