{ depot, ... }:

let

  # TODO(sterni): tests !!
  # TODO(sterni): figure out cross
  # TODO(sterni): full -ffreestanding
  # TODO(sterni): generate release tarball buildable with GNU make
  # TODO(sterni): tooling for header only libraries (split header / impl object)
  # TODO(sterni): combat SIGKILL flakiness

  inherit (depot.nix)
    runExecline
    writeExecline
    getBins
    ;

  inherit (depot.third_party)
    llvmPackages
    gcc-unwrapped
    binutils-unwrapped
    coreutils
    lib
    ;

  bins = getBins coreutils [
    "mkdir"
    "cp"
    "touch"
    "printf"
    "env"
    "test"
  ];

  # TODO(sterni): set function args?
  # for   f       :: buildConf -> derivation
  # and   initial :: buildConf
  # return a derivation that has a overrideBuildConf
  # attribute which allows overriding the initial
  # buildConf.
  buildConfOverrideable = f: initial:
    f initial // {
      overrideBuildConf = new: f (initial // new);
    };

  includeFlags =
    builtins.map (drv: "-I${lib.getDev drv}/include");

  linkFlags = static: deps: lib.concatLists
    (builtins.map (l: [
      "-L${lib.getLib l}/lib"
      "-l${l.libName}"
    ] ++ lib.optionals (!static) [
      "-rpath=${lib.getLib l}/lib"
    ]) deps);

  buildObject = { cc, ... }: flags: c:
    let
      basename = p: lib.removeSuffix ".c" (builtins.baseNameOf p);
      name =
        if builtins.isPath c
        then "${basename c}.o"
        else if lib.isDerivation c
        then "${basename c.name}.o" # avoid referring to store
        else "object.o";
    in
      runExecline name {} ([
        "importas" "out" "out" cc
      ] ++ flags ++ [
        "-o" "$out" "-c" c
      ]);

  defaultBuildConf = {
    libc = musl;            # option<libraryFromDrv>
    compiler = clangTools;  # struct
    static = false;         # bool
    commonFlags = [         # list<string>
      "-Wall"
      "-Wextra"
      # TODO(sterni): add default set of hardening flags?
    ];
  };

  library =
    { buildConf ? {}
    , name
    , libName ? name
    , include ? []
    , srcs ? []
    , deps ? []
    , CFLAGS ? []
    , LDFLAGS ? []
    , extra ? {}
    , meta ? {}
    , tests ? []
    # Argument for internal use: Allows to build the this
    # library _only_ as a static archive instead and override
    # the whole dependency tree with whatever value set for
    # static in buildConfig. This is used in program to build
    # the program into an object archive first and to then link
    # it into an actual executable. This saves a lot of code
    # duplication since a program is only a special case of
    # a library this way and still allows for dynamically
    # linked executables.
    , __forceStatic ? false
    }: let
      # a function which takes everything overrideable
      # (currently only buildConf) and returns the actual
      # derivation plus a dependency graph overridden with
      # the same settings
      f =
        { libc
        , compiler
        , static
        , commonFlags
        }@buildConf: let
        # whether we should build _this_ and only this
        # library as a static archive of object files
        thisStatic = __forceStatic || static;
        # The deps we expose need to be overridden with
        # the same buildConf as us.
        overriddenDeps = builtins.map
          (dep: dep.overrideBuildConf buildConf) deps;
        # future $out/include for this library
        includeDir = runExecline "${name}-include" {} ([
          "importas" "out" "out"
          "if" [ bins.mkdir "-p" "$out" ]
        ] ++ lib.concatMap (i:
          let
            targetName =
              if lib.isDerivation i
              then i.name
              else builtins.baseNameOf i;
          in [
          "if" [ bins.cp "--reflink=auto" "-rT" i "\${out}/${targetName}" ]
        ]) include);
        # deps, including libc if any
        allDeps = overriddenDeps
          ++ lib.optional (libc != null) (libc.overrideBuildConf buildConf);
        # CFLAGS plus additional necessary flags, include flags
        allCFLAGS = commonFlags
          ++ CFLAGS
          ++ lib.optionals (!thisStatic) [ "-fPIC" ]
          ++ includeFlags allDeps
          ++ [
            "-I${includeDir}"
          ];
        # passed LDFLAGS plus flags for dependencies
        allLDFLAGS = LDFLAGS ++ linkFlags thisStatic allDeps
          # Hack for the -lm glibc sometimes needs
          ++ (libc.extraLDFLAGS or []);
        # all built objects as individual derivations
        objs = builtins.map (buildObject compiler allCFLAGS) srcs;
        # whether any tests have to be added to the output drv
        haveTests = builtins.length tests > 0;
        # path to resulting library without file extension
        outLib = "\${out}/lib/lib${libName}";
        # main derivation without tests
        mainDrv =
          runExecline name {} ([
            "importas" "out" "out"
            "if" [ bins.mkdir "-p" "\${out}/lib" ]
            "if" [ bins.mkdir "-p" "\${out}/include" ]
            "if" [ bins.cp "--reflink=auto" "-rT" includeDir "\${out}/include" ]
          ] ++ lib.optionals thisStatic [
            "if" ([ compiler.ar "rc" "${outLib}.a" ] ++ objs)
            "if" [ compiler.ranlib "${outLib}.a" ]
          ] ++ lib.optionals (!thisStatic) [
            "if" ([
              compiler.ld
              "-shared"
              "-dynamic-linker" libc.dynamic-linker
              "-o" "${outLib}.so"
              "${lib.getLib libc}/lib/crti.o"
              "${lib.getLib libc}/lib/crtn.o"
            ] ++ objs ++ allLDFLAGS)
          ]);
        callTests = ts: builtins.map
          (t: let
            t' =
              if builtins.isFunction t
              then t (buildConfOverrideable f buildConf)
              else t;
          in
            if t' ? overrideBuildConf
            then t'.overrideBuildConf buildConf
            else t')
          tests;
      in
        mainDrv // {
          inherit libName extra;
          deps = overriddenDeps;

          meta = lib.optionalAttrs haveTests {
            targets = [ "tests" ];
          } // meta;
        } // lib.optionalAttrs haveTests {
          tests = runTests name (callTests tests);
        };
    in
      buildConfOverrideable f (defaultBuildConf // buildConf);

  # true if b depends on a
  requiredBy = a: b: builtins.elem b a.deps;

  # this is basically the same as `allDeps` from //nix/buildLisp
  # except that we need the exact reverse order in this case:
  # We need to list the libs which depend on most of the dependency
  # tree first as the linker needs to see the unresolved symbol
  # before it sees the location it is defined at.
  # Filters out any libcs as we handle that specially.
  flattenDeps = deps:
    let
      getAllDeps = drv: drv.deps ++
        lib.concatMap (dep: getAllDeps dep) drv.deps;
    in
      (lib.toposort requiredBy
        (builtins.filter (lib: lib.libName != "c")
          (lib.unique
            (lib.flatten (deps ++ builtins.map getAllDeps deps)))
        )
      ).result;

  # a program is just a static library (an archive)
  # linked into an executable with its dependencies
  program =
    { name
    , progName ? name
    , buildConf ? {}
    , ...
    }@args: let
      f =
        { static
        , libc
        , compiler
        , ...
        }@buildConf: let

          progLib = (library (args // {
            __forceStatic = true;
          })).overrideBuildConf buildConf;

          linkerFlags =
            if static
            then [ "-static" ]
            else [ "-dynamic-linker" libc.dynamic-linker ];

          allDeps =
            if !static
            then progLib.deps ++ [ libc ]
            else flattenDeps progLib.deps ++ [ libc ];

        in
          if libc == null
          then builtins.throw "Building a program without a libc is currently not supported"
          else

          runExecline name {} [
            "importas" "out" "out"
            "if" [ bins.mkdir "-p" "\${out}/bin" ]
            "if" ([
              compiler.ld
            ] ++ linkerFlags ++ [
              "-o" "\${out}/bin/${progName}"
              "${lib.getLib libc}/lib/crt1.o"
              "${lib.getLib libc}/lib/crti.o"
              "${lib.getLib libc}/lib/crtn.o"
              "${progLib}/lib/lib${progName}.a"
            ] ++ linkFlags static allDeps
              # Hack for the -lm glibc sometimes needs
              ++ (libc.extraLDFLAGS or []))
          ] // {
            inherit progName;
            inherit (progLib)
              extra
              meta
              ;
          } // lib.filterAttrs (n: _: n == "tests") progLib;
    in
      buildConfOverrideable f (defaultBuildConf // buildConf);

  runTests = name: tests: runExecline "run-all-${name}-tests" {}
    (lib.concatMap (t: [
      # force tests to be built by referencing their outpath
      "if" [ bins.test "-e" t ]
    ]) tests ++ [
      "importas" "out" "out"
      "if" [ bins.touch "$out" ]
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
      f = buildConf: let
        drv =
          (program (builtins.removeAttrs args [
            "runDeps"
            "runArgs"
          ])).overrideBuildConf buildConf;
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
            "if" [ bins.touch "$out" ]
          ];
    in
      buildConfOverrideable f {};

  libraryFromDrv =
    drv:
    { libName ? drv.name
    , deps ? []
    , supportBuildConf ? (_: true)
    , ...
    }@args: lib.fix (self:
      drv // {
        overrideBuildConf = buildConf:
          if supportBuildConf buildConf
          then self
          else builtins.throw "${drv.name} doesn't support this buildConf: ${lib.generators.toPretty {} buildConf}";
      } // args);

  musl = lib.fix (self:
    libraryFromDrv depot.third_party.musl {
      libName = "c";
      dynamic-linker = "${lib.getLib self}/lib/ld-musl-x86_64.so.1";
    });

  glibc = lib.fix (self:
    libraryFromDrv depot.third_party.glibc {
      libName = "c";
      supportBuildConf = { static, ... }: !static;
      dynamic-linker = "${lib.getLib self}/lib/ld-linux-x86-64.so.2";

      # A bit of a hack: we magically also link libm.so for glibc.
      # Rationale for this is to make the interface as compatible
      # between musl and glibc as possible. Musl doesn't have the c/m
      # split so a pseudo package for libm wouldn't make sense
      # overall.
      extraLDFLAGS = [ "-lm" ];

      # Alternative to this:
      # math = libraryFromDrv self {
      #   libName = "m";
      #   supportBuildConf = { static, ... }: !static;
      # };
    });

  clangTools =
    getBins llvmPackages.clang-unwrapped [
      { use = "clang"; as = "cc"; }
    ] // getBins llvmPackages.bintools [
      { use = "llvm-ar"; as = "ar"; }
      { use = "llvm-ranlib"; as = "ranlib"; }
      "ld"
    ];

  # necessary to make gcc find `as` at runtime apparently.
  # make sure to always test changes with clang since gcc
  # can just call stuff from binutils as it pleases
  # which clang can't do (which we want, calling exec
  # for the assembler is fine though).
  gccWrapper = writeExecline "gcc-wrapper" {} [
    "export" "PATH" "${lib.getBin binutils-unwrapped}/bin"
    "exec" "${lib.getBin gcc-unwrapped}/bin/gcc" "$@"
  ];

  gccTools =
    getBins binutils-unwrapped [
      "ld"
      "ar"
      "ranlib"
    ] // {
      cc = gccWrapper;
    };

in {
  inherit
    library
    program
    test
    libraryFromDrv
    musl
    glibc
    ;

  compilers = {
    clang = clangTools;
    gcc   = gccTools;
  };
}
