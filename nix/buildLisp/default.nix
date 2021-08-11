# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  inherit (builtins) map elemAt match filter;
  inherit (pkgs) lib runCommandNoCC makeWrapper writeText writeShellScriptBin sbcl;

  #
  # Internal helper definitions
  #

  defaultImplementation = "sbcl";

  # Process a list of arbitrary values which also contains “implementation
  # filter sets” which describe conditonal inclusion of elements depending
  # on the CL implementation used. Elements are processed in the following
  # manner:
  #
  # * Paths, strings, derivations are left as is
  # * A non-derivation attribute set is processed like this:
  #   1. If it has an attribute equal to impl.name, replace with its value.
  #   2. Alternatively use the value of the "default" attribute.
  #   3. In all other cases delete the element from the list.
  #
  # This can be used to express dependencies or source files which are specific
  # to certain implementations:
  #
  #  srcs = [
  #    # mixable with unconditional entries
  #    ./package.lisp
  #
  #    # implementation specific source files
  #    {
  #      ccl = ./impl-ccl.lisp;
  #      sbcl = ./impl-sbcl.lisp;
  #      ecl = ./impl-ecl.lisp;
  #    }
  #  ];
  #
  #  deps = [
  #    # this dependency is ignored if impl.name != "sbcl"
  #    { sbcl = buildLisp.bundled "sb-posix"; }
  #
  #    # only special casing for a single implementation
  #    {
  #      sbcl = buildLisp.bundled "uiop";
  #      default = buildLisp.bundled "asdf";
  #    }
  #  ];
  implFilter = impl: xs:
    let
      isFilterSet = x: builtins.isAttrs x && !(lib.isDerivation x);
    in builtins.map (
      x: if isFilterSet x then x.${impl.name} or x.default else x
    ) (builtins.filter (
      x: !(isFilterSet x) || x ? ${impl.name} || x ? default
    ) xs);

  # Generates lisp code which instructs the given lisp implementation to load
  # all the given dependencies.
  genLoadLispGeneric = impl: deps:
    lib.concatStringsSep "\n"
      (map (lib: "(load \"${lib}/${lib.lispName}.${impl.faslExt}\")")
        (allDeps impl deps));

  # 'genTestLispGeneric' generates a Lisp file that loads all sources and deps
  # and executes expression for a given implementation description.
  genTestLispGeneric = impl: { name, srcs, deps, expression }: writeText "${name}.lisp" ''
    ;; Dependencies
    ${impl.genLoadLisp deps}

    ;; Sources
    ${lib.concatStringsSep "\n" (map (src: "(load \"${src}\")") srcs)}

    ;; Test expression
    (unless ${expression}
      (exit :code 1))
  '';

  # 'dependsOn' determines whether Lisp library 'b' depends on 'a'.
  dependsOn = a: b: builtins.elem a b.lispDeps;

  # 'allDeps' flattens the list of dependencies (and their
  # dependencies) into one ordered list of unique deps which
  # all use the given implementation.
  allDeps = impl: deps: let
    # The override _should_ propagate itself recursively, as every derivation
    # would only expose its actually used dependencies
    deps' = builtins.map (dep: dep.overrideLisp or (lib.const dep) (_: {
      implementation = impl.name;
    })) deps;
  in (lib.toposort dependsOn (lib.unique (
    lib.flatten (deps' ++ (map (d: d.lispDeps) deps'))
  ))).result;

  # 'allNative' extracts all native dependencies of a dependency list
  # to ensure that library load paths are set correctly during all
  # compilations and program assembly.
  allNative = native: deps: lib.unique (
    lib.flatten (native ++ (map (d: d.lispNativeDeps) deps))
  );

  # Add an `overrideLisp` attribute to a function result that works
  # similar to `overrideAttrs`, but is used specifically for the
  # arguments passed to Lisp builders.
  makeOverridable = f: orig: (f orig) // {
    overrideLisp = new: makeOverridable f (orig // (new orig));
  };

  # 'testSuite' builds a Common Lisp test suite that loads all of srcs and deps,
  # and then executes expression to check its result
  testSuite = { name, expression, srcs, deps ? [], native ? [], impl }:
    let
      lispNativeDeps = allNative native deps;
      lispDeps = allDeps impl (implFilter impl deps);
      filteredSrcs = implFilter impl srcs;
    in runCommandNoCC name {
      LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
      LANG = "C.UTF-8";
    } ''
      echo "Running test suite ${name}"

      ${impl.runScript} ${
        impl.genTestLisp {
          inherit name expression;
          srcs = filteredSrcs;
          deps = lispDeps;
        }
      } | tee $out

      echo "Test suite ${name} succeeded"
    '';

  # 'impls' is an attribute set of attribute sets which describe how to do common
  # tasks when building for different Common Lisp implementations. Each
  # implementation set has the following members:
  #
  # Required members:
  #
  # - runScript :: string
  #   Describes how to invoke the implementation from the shell, so it runs a
  #   lisp file as a script and exits.
  # - faslExt :: string
  #   File extension of the implementations loadable (FASL) files.
  #   Implementations are free to generate native object files, but with the way
  #   buildLisp works it is required that we can also 'load' libraries, so
  #   (additionally) building a FASL or equivalent is required.
  # - genLoadLisp :: [ dependency ] -> string
  #   Returns lisp code to 'load' the given dependencies. 'genLoadLispGeneric'
  #   should work for most dependencies.
  # - genCompileLisp :: { name, srcs, deps } -> file
  #   Builds a lisp file which instructs the implementation to build a library
  #   from the given source files when executed. After running at least
  #   the file "$out/${name}.${impls.${implementation}.faslExt}" should have
  #   been created.
  # - genDumpLisp :: { name, main, deps } -> file
  #   Builds a lisp file which instructs the implementation to build an
  #   executable which runs 'main' (and exits) where 'main' is available from
  #   'deps'. The executable should be created as "$out/bin/${name}", usually
  #   by dumping the lisp image with the replaced toplevel function replaced.
  # - genTestLisp :: { name, srcs, deps, expression } -> file
  #   Builds a lisp file which loads the given 'deps' and 'srcs' files and
  #   then evaluates 'expression'. Depending on whether 'expression' returns
  #   true or false, the script must exit with a zero or non-zero exit code.
  #   'genTestLispGeneric' will work for most implementations.
  # - lispWith :: [ dependency ] -> drv
  #   Builds a script (or dumped image) which when executed loads (or has
  #   loaded) all given dependencies. When built this should create an executable
  #   at "$out/bin/${implementation}".
  #
  # Optional members:
  #
  # - bundled :: string -> library
  #   Allows giving an implementation specific builder for a bundled library.
  #   This function is used as a replacement for the internal defaultBundled
  #   function and only needs to support one implementation. The returned derivation
  #   must behave like one built by 'library' (in particular have the same files
  #   available in "$out" and the same 'passthru' attributes), but may be built
  #   completely differently.
  impls = lib.mapAttrs (name: v: { inherit name; } // v) {
    sbcl = {
      runScript = "${sbcl}/bin/sbcl --script";
      faslExt = "fasl";

      # 'genLoadLisp' generates Lisp code that instructs SBCL to load all
      # the provided Lisp libraries.
      genLoadLisp = genLoadLispGeneric impls.sbcl;

      # 'genCompileLisp' generates a Lisp file that instructs SBCL to
      # compile the provided list of Lisp source files to "$out/${name}.fasl".
      genCompileLisp = { name, srcs, deps }: writeText "sbcl-compile.lisp" ''
        ;; This file compiles the specified sources into the Nix build
        ;; directory, creating one FASL file for each source.
        (require 'sb-posix)

        ${impls.sbcl.genLoadLisp deps}

        (defun nix-compile-lisp (srcfile)
          (let ((outfile (make-pathname :type "fasl"
                                        :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                       (error "not running in a Nix build"))
                                        :name (substitute #\- #\/ srcfile))))
            (multiple-value-bind (out-truename _warnings-p failure-p)
                (compile-file srcfile :output-file outfile)
              (if failure-p (sb-posix:exit 1)
                  (progn
                    ;; For the case of multiple files belonging to the same
                    ;; library being compiled, load them in order:
                    (load out-truename)

                    ;; Return pathname as a string for cat-ting it later
                    (namestring out-truename))))))

        (let ((*compile-verbose* t)
              (catted-fasl (make-pathname :type "fasl"
                                          :directory (or (sb-posix:getenv "out")
                                                         (error "not running in a Nix build"))
                                          :name "${name}")))

          (with-open-file (file catted-fasl
                                :direction :output
                                :if-does-not-exist :create)

            ;; SBCL's FASL files can just be bundled together using cat
            (sb-ext:run-program "cat"
             (mapcar #'nix-compile-lisp
              ;; These forms were inserted by the Nix build:
              '(${
                lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs
              }))
             :output file :search t)))
      '';

      # 'genDumpLisp' generates a Lisp file that instructs SBCL to dump
      # the currently loaded image as an executable to $out/bin/$name.
      #
      # TODO(tazjin): Compression is currently unsupported because the
      # SBCL in nixpkgs is, by default, not compiled with zlib support.
      genDumpLisp = { name, main, deps }: writeText "sbcl-dump.lisp" ''
        (require 'sb-posix)

        ${impls.sbcl.genLoadLisp deps}

        (let* ((bindir (concatenate 'string (sb-posix:getenv "out") "/bin"))
               (outpath (make-pathname :name "${name}"
                                       :directory bindir)))
          (save-lisp-and-die outpath
                             :executable t
                             :toplevel (function ${main})
                             :purify t))
      '';

      genTestLisp = genTestLispGeneric impls.sbcl;

      lispWith = deps:
        let lispDeps = filter (d: !d.lispBinary) (allDeps impls.sbcl deps);
        in writeShellScriptBin "sbcl" ''
          export LD_LIBRARY_PATH="${lib.makeLibraryPath (allNative [] lispDeps)}"
          export LANG="C.UTF-8"
          exec ${sbcl}/bin/sbcl ${
            lib.optionalString (deps != [])
              "--load ${writeText "load.lisp" (impls.sbcl.genLoadLisp lispDeps)}"
          } $@
        '';
    };
  };

  #
  # Public API functions
  #

  # 'library' builds a list of Common Lisp files into an implementation
  # specific library format, usually a single FASL file, which can then be
  # loaded and built into an executable via 'program'.
  library =
    { name
    , implementation ? defaultImplementation
    , srcs
    , deps ? []
    , native ? []
    , tests ? null
    }:
    let
      impl = impls."${implementation}" or
        (builtins.throw "Unkown Common Lisp Implementation ${implementation}");
      filteredDeps = implFilter impl deps;
      filteredSrcs = implFilter impl srcs;
      lispNativeDeps = (allNative native filteredDeps);
      lispDeps = allDeps impl filteredDeps;
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs = filteredSrcs ++ (tests.srcs or []);
          deps = filteredDeps ++ (tests.deps or []);
          expression = tests.expression;
          inherit impl;
        }
        else null;
    in lib.fix (self: runCommandNoCC "${name}-cllib" {
      LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
      LANG = "C.UTF-8";
      passthru = {
        inherit lispNativeDeps lispDeps;
        lispName = name;
        lispBinary = false;
        tests = testDrv;
        ${implementation} = impl.lispWith [ self ];
      };
    } ''
      ${if ! isNull testDrv
        then "echo 'Test ${testDrv} succeeded'"
        else "echo 'No tests run'"}

      mkdir $out

      ${impl.runScript} ${
        impl.genCompileLisp {
          srcs = filteredSrcs;
          inherit name;
          deps = lispDeps;
        }
      }
    '');

  # 'program' creates an executable, usually containing a dumped image of the
  # specified sources and dependencies.
  program =
    { name
    , implementation ? defaultImplementation
    , main ? "${name}:main"
    , srcs
    , deps ? []
    , native ? []
    , tests ? null
    }:
    let
      impl = impls."${implementation}" or
        (builtins.throw "Unkown Common Lisp Implementation ${implementation}");
      filteredSrcs = implFilter impl srcs;
      filteredDeps = implFilter impl deps;
      lispDeps = allDeps impl filteredDeps;
      libPath = lib.makeLibraryPath (allNative native lispDeps);
      # overriding is used internally to propagate the implementation to use
      selfLib = (makeOverridable library) {
        inherit name native;
        deps = lispDeps;
        srcs = filteredSrcs;
      };
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs =
            ( # testSuite does run implFilter as well
              filteredSrcs ++ (tests.srcs or []));
          deps = filteredDeps ++ (tests.deps or []);
          expression = tests.expression;
          inherit impl;
        }
        else null;
    in lib.fix (self: runCommandNoCC "${name}" {
      nativeBuildInputs = [ makeWrapper ];
      LD_LIBRARY_PATH = libPath;
      LANG = "C.UTF-8";
      passthru = {
        lispName = name;
        lispDeps = [ selfLib ];
        lispNativeDeps = native;
        lispBinary = true;
        tests = testDrv;
        ${implementation} = impl.lispWith [ self ];
      };
    } ''
      ${if ! isNull testDrv
        then "echo 'Test ${testDrv} succeeded'"
        else ""}
      mkdir -p $out/bin

      ${impl.runScript} ${
        impl.genDumpLisp {
          inherit name main;
          deps = ([ selfLib ] ++ lispDeps);
        }
      }

      wrapProgram $out/bin/${name} --prefix LD_LIBRARY_PATH : "${libPath}"
    '');

  # 'bundled' creates a "library" which makes a built-in package available,
  # such as any of SBCL's sb-* packages or ASDF. By default this is done
  # by calling 'require', but implementations are free to provide their
  # own specific bundled function.
  bundled = name:
    let
      # TODO(sterni): allow overriding args to underlying 'library' (e. g. srcs)
      defaultBundled = implementation: name: library {
        inherit name implementation;
        srcs = lib.singleton (builtins.toFile "${name}.lisp" "(require '${name})");
      };

      bundled' =
        { implementation ? defaultImplementation
        , name
        }:
        impls."${implementation}".bundled or (defaultBundled implementation) name;

    in (makeOverridable bundled') {
      inherit name;
    };

in {
  library = makeOverridable library;
  program = makeOverridable program;
  inherit bundled;

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded in SBCL.
  sbclWith = impls.sbcl.lispWith;
}
