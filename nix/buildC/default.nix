{ depot, ... }:

let

  inherit (depot.nix)
    runExecline
    getBins
    ;

  inherit (depot.third_party)
    llvmPackages
    gcc-unwrapped
    binutils-unwrapped
    coreutils
    lib
    writers
    ;

  clangBins =
       getBins llvmPackages.clang-unwrapped [ "clang" ]
    // getBins llvmPackages.bintools [ "llvm-ar" "llvm-ranlib" "ld" ]
    ;

  gccBins =
       getBins binutils-unwrapped [ "ld" "ar" "ranlib" ]
    // getBins gccWrapper [ "gcc" ]
    ;

  # necessary to make gcc find as at runtime apparently
  # make sure to always test changes with clang as gcc
  # can just call stuff from binutils as it pleases
  # which clang can't do (which we want, calling exec
  # for the assembler is fine though).
  gccWrapper = writers.writeDashBin "gcc" ''
    export PATH='${lib.getBin binutils-unwrapped}/bin':''${PATH:+':'}$PATH
    ${lib.getBin gcc-unwrapped}/bin/gcc $@
  '';

  bins = getBins coreutils [ "mkdir" "cp" ];

  compilerTools = {
    clang = {
      cc = clangBins.clang;
      ld = clangBins.ld;
      ar = clangBins.llvm-ar;
      ranlib = clangBins.llvm-ranlib;
    };
    gcc = {
      cc = gccBins.gcc;
      ar = gccBins.ar;
      ld = gccBins.ld;
      ranlib = gccBins.ranlib;
    };
  };

  # TODO(sterni): test support
  # TODO(sterni): generate release tarball buildable with GNU make

  includeFlags = builtins.map (i: "-I${i}");

  depFlags = deps: lib.flatten
    (builtins.map (l: [
      "-L${lib.getLib l}/lib"
      "-l${l.name}"
      "-rpath=${lib.getLib l}/lib"
    ]) deps);

  buildObject = { cc, ... }: flags: c:
    let
      basename = lib.removeSuffix ".c" (builtins.baseNameOf c);
    in
      runExecline "${basename}.o" {} ([
        "importas" "out" "out" cc
      ] ++ flags ++ [
        "-o" "$out" "-c" c
      ]);

  libraryFromDrv =
    { name
    , deps ? []
    }: drv:
    drv // {
      inherit name deps;
    };

  musl = lib.fix (self:
    libraryFromDrv { name = "c"; } depot.third_party.musl // {
      dynamic-linker = "${lib.getLib self}/lib/ld-musl-x86_64.so.1";
    });

  glibc = lib.fix (self:
    libraryFromDrv { name = "c"; } depot.third_party.glibc // {
      dynamic-linker = "${lib.getLib self}/lib/ld-linux-x86-64.so.2";
    });

  library =
    { tools ? compilerTools.clang
    , libc ? musl
    , lc ? true
    , include ? []
    , srcs ? []
    , deps ? []
    , CFLAGS ? []
    , LDFLAGS ? []
    , name
    , extra ? {}
    , static ? false
    }: let
      # deps, including libc if any
      allDeps = lib.optional lc libc ++ deps;
      # CFLAGS plus additional necessary flags, include flags
      allCFLAGS = CFLAGS ++ includeFlags ([
        includeDir
      ] ++ builtins.map (d: "${lib.getDev d}/include") allDeps)
        ++ lib.optional (!static) "-fPIC";
      # passed LDFLAGS plus flags for dependencies
      allLDFLAGS = LDFLAGS
        ++ depFlags allDeps;
      # all built objects as individual derivations
      objs = builtins.map (buildObject tools allCFLAGS) srcs;
      # path to resulting library without file extension
      outLib = "\${out}/lib/lib${name}";
      # future $out/include for this library
      includeDir = runExecline "${name}-include" {} ([
        "importas" "out" "out"
        "if" [ bins.mkdir "-p" "$out" ]
      ] ++ lib.concatMap (i: [
        "if" [ bins.cp "--reflink=auto" "-rT" i "\${out}/${builtins.baseNameOf i}" ]
      ]) include);
    in
      runExecline name {
      } ([
        "importas" "out" "out"
        "if" [ bins.mkdir "-p" "\${out}/lib" ]
        "if" [ bins.mkdir "-p" "\${out}/include" ]
        "if" [ bins.cp "--reflink=auto" "-rT" includeDir "\${out}/include" ]
      ] ++ lib.optionals static [
        "if" ([ tools.ar "rc" "${outLib}.a" ] ++ objs)
        "if" [ tools.ranlib "${outLib}.a" ]
      ] ++ lib.optionals (!static) [
        "if" ([
          tools.ld
          "-shared"
          "-dynamic-linker" libc.dynamic-linker
          "-o" "${outLib}.so"
          "${lib.getLib libc}/lib/crti.o"
          "${lib.getLib libc}/lib/crtn.o"
        ] ++ objs ++ allLDFLAGS)
      ]) // {
        inherit deps extra;
      };

  # a program is just a static library (an archive)
  # linked into an executable with its dependencies
  program =
    { tools ? compilerTools.clang
    , libc ? musl
    , lc ? true
    , name
    , extra ? {}
    , static ? false # TODO(sterni): static linking
    , ...
    }@args:
    assert (!static);
    let
      progLib = library (args // { static = true; });
    in
      runExecline name {} [
        "importas" "out" "out"
        "if" [ bins.mkdir "-p" "\${out}/bin" ]
        "if" ([
          tools.ld
          "-dynamic-linker" libc.dynamic-linker
          "-o" "\${out}/bin/${name}"
          "${lib.getLib libc}/lib/crt1.o"
          "${lib.getLib libc}/lib/crti.o"
          "${lib.getLib libc}/lib/crtn.o"
          "${progLib}/lib/lib${name}.a"
        ] ++ lib.optionals lc (depFlags [ libc ])
          ++ depFlags progLib.deps)
      ] // {
        inherit extra;
      };

in {
  inherit
    library
    program
    libraryFromDrv
    ;

  tools = compilerTools;
}
