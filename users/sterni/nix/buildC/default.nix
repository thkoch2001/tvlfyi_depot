{ depot, pkgs, lib, ... }:

let

  # TODO(sterni): test support

  # for some reason runCommand' is not exposed in <nixpkgs>
  runCommandInStdenv =  stdenv: name: env: buildCommand:
    stdenv.mkDerivation ({
      inherit name buildCommand;
      passAsFile = [ "buildCommand" ];
    } // env);

  inherit (pkgs)
    runCommandNoCC
    ;

  includeFlags = builtins.map (i: "-I${i}");

  buildObject = stdenv: flags: c:
    let
      basename = lib.removeSuffix ".c" (builtins.baseNameOf c);
    in runCommandInStdenv stdenv "${basename}.o" {} ''
      $CC ${lib.escapeShellArgs flags} -o "$out" -c ${c}
    '';

  library =
    { stdenv ? pkgs.llvmPackages.stdenv
    , include ? []
    , src ? []
    , CFLAGS ? []
    , pname
    , version ? "unstable"
    , extra ? {}
    }: let
      allCFLAGS = CFLAGS
        ++ includeFlags include;
      objs = builtins.map
        (buildObject stdenv allCFLAGS) src;
      outLib = "\"$out/lib/lib${pname}.a\"";
    in runCommandInStdenv stdenv pname {
      passthru = { inherit extra; };
      inherit pname;
    } (''
      mkdir -p "$out/lib"
      $AR rc ${outLib} ${lib.escapeShellArgs objs}
      $RANLIB ${outLib}
    '' + lib.concatMapStrings (i: ''
      cp -r --reflink=auto "${i}" "$out/include"
    '') include);

  program =
    { stdenv ? pkgs.llvmPackages.stdenv
    , include ? []
    , libraries ? []
    , src ? []
    , CFLAGS ? []
    , pname
    , version ? "unstable"
    , extra ? {}
    }: let
      allCFLAGS = CFLAGS
        ++ includeFlags include
        ++ includeFlags (builtins.map (l: "${l}/include") libraries);
      LDFLAGS = lib.flatten (builtins.map
        (l: [ "-L${l}/lib" "-l${l.pname}" ]) libraries);
      objs = builtins.map
        (buildObject stdenv allCFLAGS) src;
    # TODO(sterni): w/o $CC
    in runCommandInStdenv stdenv pname {
      passthru = { inherit extra; };
      inherit pname;
    } ''
      mkdir -p "$out/bin"
      $CC ${lib.escapeShellArgs allCFLAGS} \
        -o "$out/bin/${pname}" \
        ${lib.escapeShellArgs objs} \
        ${lib.escapeShellArgs LDFLAGS}
    '';

in {
  inherit
    library
    program
    ;
}
