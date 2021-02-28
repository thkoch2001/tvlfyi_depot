{ depot, ... }:

let

  # TODO(sterni): cross
  # TODO(sterni): generate release tarball buildable with GNU make
  # TODO(sterni): tooling for header only libraries (split header / impl object)

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

  # necessary to make gcc find `as` at runtime apparently
  # make sure to always test changes with clang as gcc
  # can just call stuff from binutils as it pleases
  # which clang can't do (which we want, calling exec
  # for the assembler is fine though).
  gccWrapper = writers.writeDashBin "gcc" ''
    export PATH='${lib.getBin binutils-unwrapped}/bin':''${PATH:+':'}$PATH
    ${lib.getBin gcc-unwrapped}/bin/gcc $@
  '';

  bins = getBins coreutils [ "mkdir" "cp" "touch" "printf" "env" "test" ];

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
      name =
        if builtins.isPath c
        then "${basename}.o"
        else "object.o"; # dodge referring to store path errors
    in
      runExecline name {} ([
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
    , meta ? {}
    , tests ? []
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
      # whether any tests have to be added to the output drv
      haveTests = builtins.length tests > 0;
      # main derivation without tests
      mainDrv =
        runExecline name {} ([
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
        ]);
      callTests = ts: builtins.map
        (t: if builtins.isFunction t then t mainDrv else t)
        tests;
    in
      mainDrv // {
        inherit deps extra;
        meta = lib.optionalAttrs haveTests {
          targets = [ "tests" ];
        } // meta;
      } // lib.optionalAttrs haveTests {
        tests = runTests name (callTests tests);
      };

  # a program is just a static library (an archive)
  # linked into an executable with its dependencies
  program =
    { tools ? compilerTools.clang
    , libc ? musl
    , lc ? true
    , name
    , static ? false # TODO(sterni): actual static linking
    , ...
    }@args:
    let
      progLib = library (args // { static = true; });
      linkFlags =
        if static
        then [ "-static" ]
        else [ "-dynamic-linker" libc.dynamic-linker ];
    in
      runExecline name {} [
        "importas" "out" "out"
        "if" [ bins.mkdir "-p" "\${out}/bin" ]
        "if" ([
          tools.ld
        ] ++ linkFlags ++ [
          "-o" "\${out}/bin/${name}"
          "${lib.getLib libc}/lib/crt1.o"
          "${lib.getLib libc}/lib/crti.o"
          "${lib.getLib libc}/lib/crtn.o"
          "${progLib}/lib/lib${name}.a"
        ] ++ lib.optionals lc (depFlags [ libc ])
          ++ depFlags progLib.deps)
      ] // {
        inherit (progLib)
          extra
          meta
          ;
      } // lib.filterAttrs (n: _: n == "tests") progLib;

  runTests = name: tests: runExecline "run-all-${name}-tests" {}
    (lib.concatMap (t: [
      # force tests to be built by referencing their outpath
      "if" [ bins.test "-e" t ]
    ]) tests ++ [
      "importas" "out" "out"
      bins.touch "$out"
    ]);

  # a test is a program which is executed with runArgs
  # in an environment which has runDeps available
  test =
    { name
    , runDeps ? []
    , runArgs ? []
    , ...
    }@args:
    let
      drv = args.drv or
        program (builtins.removeAttrs args [ "runDeps" ]);
    in
      runExecline "run-${name}" {} [
        "importas" "out" "out"
        "if" ([
          bins.env "PATH=${lib.makeBinPath runDeps}"
          "${lib.getBin drv}/bin/${name}"
        ] ++ runArgs)
        "if" [
          bins.printf
          "ran test %s (%s/bin/%s)"
          drv.name
          drv
          drv.name
        ]
        bins.touch "$out"
      ];

in {
  inherit
    library
    program
    test
    libraryFromDrv
    musl
    glibc
    ;

  tools = compilerTools;
}
