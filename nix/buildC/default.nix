{ depot, pkgs, lib, ... }:

let

  # TODO(sterni): figure out cross, maybe reuse pkgsCross by adding a compilerFor stdenv
  # feat(sterni): tooling for header only libraries (split header / impl object)
  # feat(sterni): generate release tarball buildable with GNU make (cursed but useful)

  inherit (depot.nix)
    runExecline
    writeExecline
    getBins
    utils
    ;

  inherit (pkgs)
    llvmPackages
    gcc-unwrapped
    binutils-unwrapped
    coreutils
    ;

  bins = getBins coreutils [
    "mkdir"
    "cp"
    "touch"
    "printf"
    "env"
    "test"
  ];

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

  # For a given compiler, list of CFLAGS and a C source file,
  # return a derivation that builds an object file from that.
  buildObject = { cc, ... }: flags: c:
    let
      basename = p: lib.removeSuffix ".c" (builtins.baseNameOf p);
      name = "${lib.removeSuffix ".c" (utils.storePathName c)}.o";
    in
      runExecline name {} ([
        "importas" "out" "out" cc
      ] ++ flags ++ [
        "-o" "$out" "-c" c
      ]);

  # A buildConf has the following structure:
  #
  # {
  #   libc = libraryFromDrv libc {
  #     libName = "c";
  #     dynamic-linker = path to dynamic linker;
  #     extraLDFLAGS = list of flags;
  #     …
  #   };
  #   compiler = {
  #     cc = path to C compiler;
  #     ld = path to linker;
  #     ar = path to ar;
  #     ranlib = path to ranlib;
  #     # these are always passed if present to
  #     # allow stuff like target specification
  #     # for clang via -target
  #     CFLAGS = list of flags;
  #   };
  #   static :: bool (whether to statically link)
  #   commonFlags = list of CFLAGS to always use
  # }
  #
  # Most of this are implementation details the user
  # should pass buildC.compilers.xyz for compiler and
  # either buildC.musl or buildC.glibc for libc.
  defaultBuildConf = {
    libc = musl;
    compiler = clangTools;
    static = false;
    commonFlags = [
      "-Wall"
      "-Wextra"
      # TODO(sterni): add default set of hardening flags?
    ];
  };

  library =
    { # general build settings which will affect this derivation
      # and all derivation it depends on. If the user provides
      # a buildConf it is used to update the default buildConf,
      # so it can be omitted or only given partially.
      #
      # Valid settings are:
      #
      # * libc: derivation with extra meta information.
      #   buildC.musl and buildC.glibc are supported.
      # * compiler: special set describing compiler and bitools
      #   to use. Valid choices are buildC.compilers.clang and
      #   buildC.compilers.gcc.
      # * static: if true, link statically and build libraries
      #   as object archives.
      # * commonFlags: CFLAGS to pass to every invocation of the
      #   C compiler.
      buildConf ? {}
      # name of the resulting derivation
    , name
      # name of the library to build and install:
      # The resulting library will be name lib${libName}.{so,a}
      # and -l${libName} will be passed to ld.
    , libName ? name
      # Header files to install to $out/include which are also
      # made available with -I during compilation as paths or
      # derivations. There is currently no distinction between
      # public headers and private ones which are only available
      # at compile time.
      # Note that all headers specified in this list are copied
      # to $out/include/<basename> always, so if you have
      # subdirectories of headers you currently have to specify
      # the whole directory here currently. This should be
      # improved in the future.
      # Any changes in headers causes a full rebuild as it is
      # considered an interface change. While this is often too
      # aggressive it means we don't have to specify or detect
      # dependencies.
    , include ? []
      # List of C source files to be compiled, as paths or derivations.
    , srcs ? []
      # Dependencies of this library. must be built with buildC.library
      # or buildC.libraryFromDrv.
    , deps ? []
      # Extra CFLAGS to be used when compiling this.
    , CFLAGS ? []
      # extra allows passing through an attribute set of extra values
      # which will be available via the extra attribute of the resulting
      # derivation.
    , extra ? {}
      # meta set for the resulting derivation.
    , meta ? {}
      # A test is a derivation or a function which receives the library
      # as the single argument and returns a derivation. All tests can
      # be built in sequence using the tests attribute of the resulting
      # derivation and the test execution is added to the meta.targets
      # list (if it is not specified manually by the user). buildC.test
      # provides a convenience wrapper to build and execute a binary
      # test suite. A common usage of tests looks like this:
      #
      # {
      #   name = "mylib";
      #   …
      #   tests = [
      #     (mylib: buildC.test {
      #       …
      #       deps = [ mylib ];
      #     })
      #   ];
      # }
    , tests ? []
    # Argument for internal use: Allows to build the this
    # library _only_ as a static archive instead and
    # independently of the value used for the dependency tree.
    # This is used in program to build the program into an object
    # archive first and to then link it into an actual executable.
    # This saves a lot of code duplication since a program is
    # only a special case of a library this way and still allows
    # for dynamically linked executables.
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
        ]) include ++ [
          "exit" "0"
        ]);
        # deps, including libc if any
        allDeps = overriddenDeps
          ++ lib.optional (libc != null) (libc.overrideBuildConf buildConf);
        # CFLAGS plus additional necessary flags, include flags
        allCFLAGS = commonFlags
          ++ CFLAGS
          ++ (compiler.CFLAGS or [])
          ++ lib.optionals (!thisStatic) [ "-fPIC" ]
          ++ includeFlags allDeps
          ++ [
            "-I${includeDir}"
          ];
        # passed LDFLAGS for dependencies
        allLDFLAGS = linkFlags thisStatic allDeps
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

          # Add derivation that runs the tests to meta.targets,
          # so they are discovered by readTree and gather when
          # building the CI pipeline.
          # TODO(sterni): drvSeq tests with the main derivation?
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
    { # name of the derivation
      name
      # name of the binary to be linked and installed to `$out/bin`
    , progName ? name
      # build configuration to use, see documentation for `buildC.library`.
    , buildConf ? {}
      # other options are like `buildC.library`.
    , ...
    }@args: let
      f =
        { static
        , libc
        , compiler
        , ...
        }@buildConf: let

          progLib = (library (builtins.removeAttrs args [ "progName" ] // {
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
              "${progLib}/lib/lib${progLib.libName}.a"
            ] ++ linkFlags static allDeps
              # Hack for the -lm glibc sometimes needs
              ++ (libc.extraLDFLAGS or []))
          ] // {
            inherit progName;
            inherit (progLib)
              extra
              meta
              deps
              ;
          } // lib.filterAttrs (n: _: n == "tests") progLib;
    in
      buildConfOverrideable f (defaultBuildConf // buildConf);

  runTests = name: tests: runExecline "run-all-${name}-tests" {}
    (lib.concatMap (t: [
      # force tests to be built by referencing their outpath
      "if" [ bins.test "-e" t ]
    ]) tests

    ++ [
      "importas" "out" "out"
      "if" [ bins.touch "$out" ]
      "exit" "0"
    ]);

  # a test is a program which is executed with runArgs
  # in an environment which has runDeps available
  test =
    { # name of the derivation building the test binary
      name
      # name of the test binary
    , progName ? name
      # derivations available at runtime for the test binary.
      # Currently derivations in here get added to `PATH`.
    , runDeps ? []
      # arguments to pass to the test binary.
    , runArgs ? []
      # other options are like `buildC.library`.
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
              "${lib.getBin drv}/bin/${progName}"
            ] ++ runArgs)
            "if" [
              bins.printf
              "ran test %s (%s/bin/%s)"
              drv.name
              drv
              progName
            ]
            "if" [ bins.touch "$out" ]
            "exit" "0"
          ];
    in
      buildConfOverrideable f {};

  # Allows attaching extra meta data to a normal derivation
  # in order to make it usable as a dependency for a `buildC`
  # derivation.
  libraryFromDrv =
    # derivation to use
    drv:
    { # library name, `${lib.getLib drv}/lib/lib${libName}.so`
      # (or `.a` for static linking) needs to exist.
      libName ? drv.name
      # dependencies of the library to propagate in case of
      # static linking. Not relevant for dynamic linking.
    , deps ? []
      # Predicate which receives a `buildConf` attribute set
      # and should return whether that configuration is supported
      # by the derivation. This is used when `overrideBuildConf`
      # is called for the resulting `library`: If it returns
      # `false` for the passed `buildConf`, `overrideBuildConf`
      # will throw an error.
      #
      # For example, if your derivation does not install a
      # static archive use `{ static, ... }: !static`.
    , supportBuildConf ? (_: true)
      # extra arguments passed behave as if they where added
      # to `passthru`.
    , ...
    }@args: lib.fix (self:
      drv // {
        overrideBuildConf = buildConf:
          if supportBuildConf buildConf
          then self
          else builtins.throw "${drv.name} doesn't support this buildConf: ${lib.generators.toPretty {} buildConf}";
      } // args);

  musl = lib.fix (self:
    libraryFromDrv pkgs.musl {
      libName = "c";
      dynamic-linker = "${lib.getLib self}/lib/ld-musl-x86_64.so.1";
    });

  glibc = lib.fix (self:
    libraryFromDrv pkgs.glibc {
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

  # mimic the behavior of the cc wrapper without
  # actually depending on the derivation
  clangResourceDir =
    llvmPackages:
    let
      version = llvmPackages.clang.version;
    in
    pkgs.runCommand "clang-resource-dir-${version}" {} ''
      mkdir -p "$out"
      ln -s "${lib.getLib llvmPackages.compiler-rt}/lib" "$out/lib"
      ln -s "${lib.getLib llvmPackages.compiler-rt}/share" "$out/share"
      ln -s "${lib.getLib llvmPackages.clang-unwrapped}/lib/clang/${version}/include" "$out/include"
    '';

  clangTools =
    getBins llvmPackages.clang-unwrapped [
      { use = "clang"; as = "cc"; }
    ] // getBins llvmPackages.bintools-unwrapped [
      { use = "llvm-ar"; as = "ar"; }
      { use = "llvm-ranlib"; as = "ranlib"; }
      "ld"
    ] // {
      CFLAGS = [
        "-resource-dir=${clangResourceDir llvmPackages}"
      ];
    };

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
    defaultBuildConf
    ;

  compilers = {
    clang = clangTools;
    gcc   = gccTools;
  };

  libcs = {
    inherit
      glibc
      musl
      ;
  };
}
