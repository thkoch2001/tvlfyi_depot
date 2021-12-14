{ pkgs, ... }:

with pkgs.lib;

let
  inherit (pkgs) gitignoreSource;

  deps = import ./deps.nix {
    inherit (pkgs) fetchMavenArtifact fetchgit lib;
  };
in rec {
  meta.targets = [
    "db-util"
    "server"
  ];

  depsPaths = deps.makePaths {};

  resources = builtins.filterSource (_: type: type != "symlink") ./resources;

  classpath.dev = concatStringsSep ":" (
    (map gitignoreSource [./src ./test ./env/dev]) ++ [resources] ++ depsPaths
  );

  classpath.test = concatStringsSep ":" (
    (map gitignoreSource [./src ./test ./env/test]) ++ [resources] ++ depsPaths
  );

  classpath.prod = concatStringsSep ":" (
    (map gitignoreSource [./src ./env/prod]) ++ [resources] ++ depsPaths
  );

  testClojure = pkgs.writeShellScript "test-clojure" ''
    export HOME=$(pwd)
    ${pkgs.clojure}/bin/clojure -Scp ${depsPaths}
  '';

  mkJar = name: opts:
    with pkgs;
    assert (hasSuffix ".jar" name);
    stdenv.mkDerivation rec {
      inherit name;
      dontUnpack = true;
      buildPhase = ''
        export HOME=$(pwd)
        cp ${./pom.xml} pom.xml
        cp ${./deps.edn} deps.edn
        ${clojure}/bin/clojure \
          -Scp ${classpath.prod} \
          -A:uberjar \
          ${name} \
          -C ${opts}
      '';

      doCheck = true;

      checkPhase = ''
        echo "checking for existence of ${name}"
        [ -f ${name} ]
      '';

      installPhase = ''
        cp ${name} $out
      '';
    };

  db-util-jar = mkJar "bbbg-db-util.jar" "-m bbbg.db";

  db-util = pkgs.writeShellScriptBin "bbbg-db-util" ''
    exec ${pkgs.openjdk17_headless}/bin/java -jar ${db-util-jar} "$@"
  '';

  server-jar = mkJar "bbbg-server.jar" "-m bbbg.core";

  server = pkgs.writeShellScriptBin "bbbg-server" ''
    exec ${pkgs.openjdk17_headless}/bin/java -jar ${server-jar} "$@"
  '';
}
