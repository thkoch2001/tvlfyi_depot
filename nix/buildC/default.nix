{ depot, pkgs, lib, ... }:

let

  # TODO(sterni): test support
  # TODO(sterni): generate release tarball buildable with GNU make
  # TODO(sterni): without stdenv?

  # for some reason runCommand' is not exposed in <nixpkgs>
  runCommandInStdenv =  stdenv: name: env: buildCommand:
    stdenv.mkDerivation ({
      inherit name buildCommand;
      passAsFile = [ "buildCommand" ];
    } // env);

  includeFlags = builtins.map (i: "-I${i}");

  depFlags = deps: lib.flatten
    (builtins.map (l: [ "-L${l}/lib" "-l${l.name}" ]) deps);

  buildObject = stdenv: flags: c:
    let
      basename = lib.removeSuffix ".c" (builtins.baseNameOf c);
    in runCommandInStdenv stdenv "${basename}.o" {} ''
      $CC ${lib.escapeShellArgs flags} -o "$out" -c ${c}
    '';

  libraryFromDrv =
    { name
    , deps ? []
    }: drv:
    drv // {
      inherit name deps;
    };

  library =
    { stdenv ? pkgs.llvmPackages.stdenv
    , include ? []
    , srcs ? []
    , deps ? []
    , CFLAGS ? []
    , LDFLAGS ? []
    , name
    , extra ? {}
    , static ? false
    }: let
      allCFLAGS = CFLAGS
        ++ includeFlags include
        ++ includeFlags (builtins.map (d: "${d}/include") deps);
      allLDFLAGS = LDFLAGS
        ++ depFlags deps;
      objs = builtins.map
        (buildObject stdenv allCFLAGS) srcs;
      outLib = "\"$out/lib/lib${name}\"";
    in runCommandInStdenv stdenv name {
      passthru = { inherit extra deps; };
    } (''
      mkdir -p "$out/lib"
    '' + lib.optionalString static ''
      $AR rc ${outLib}.a ${lib.escapeShellArgs objs}
      $RANLIB ${outLib}.a
    '' + lib.optionalString (!static) ''
      $CC -shared -o ${outLib}.so ${lib.escapeShellArgs objs} \
        ${lib.escapeShellArgs allLDFLAGS}
    '' + lib.concatMapStrings (i: ''
      cp -r --reflink=auto "${i}" "$out/include"
    '') include);

  # a program is just a static library (an archive)
  # linked into an executable with its dependencies
  # TODO(sterni): investigate fully statically linked
  #               executables. Only possible with pkgsStatic?
  program =
    { stdenv ? pkgs.llvmPackages.stdenv
    , name
    , extra ? {}
    , ...
    }@args:
    let
      progLib = library (args // { static = true; });
    in runCommandInStdenv stdenv name {
      passthru = { inherit extra; };
    } ''
      mkdir -p "$out/bin"
      $CC -o "$out/bin/${name}" "${progLib}/lib/lib${name}.a" \
        ${lib.escapeShellArgs (depFlags progLib.deps)}
    '';

in {
  inherit
    library
    program
    libraryFromDrv
    ;
}
