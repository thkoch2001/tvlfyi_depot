# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  inherit (builtins) map elemAt match filter;
  inherit (pkgs) lib runCommandNoCC makeWrapper writeText writeShellScriptBin sbcl ecl-static;
  inherit (lib.strings) isCoercibleToString;

  #
  # Internal helper definitions
  #

  defaultImplementation = "sbcl";

  filterSrcs = impl: srcs: builtins.map (
    src: if isCoercibleToString src then src else src.${impl.name}
  ) (builtins.filter (
    src: isCoercibleToString src || src ? ${impl.name}
  ) srcs);

  # Generates lisp code which instructs the given lisp implementation to load
  # all the given dependencies.
  genLoadLispGeneric = impl: deps:
    lib.concatStringsSep "\n"
      (map (lib: "(load \"${lib}/${lib.lispName}.${impl.faslExt}\")")
        (allDepsForImpl impl deps));

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
  # dependencies) into one ordered list of unique deps.
  allDeps = deps: (lib.toposort dependsOn (lib.unique (
    lib.flatten (deps ++ (map (d: d.lispDeps) deps))
  ))).result;

  # 'allDepsForImpl' is like 'allDeps' but also makes sure that the given
  # implementation is used across the dependency graph. This is almost always
  # required since implementations usually can't load other implementations
  # FASL files.
  allDepsForImpl = impl: deps: builtins.map (
    dep: if dep ? overrideLisp
         then dep.overrideLisp (_: { implementation = impl.name; })
         else dep
  ) (allDeps deps);

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
      lispDeps = allDeps deps;
      srcsForImpl = filterSrcs impl srcs;
    in runCommandNoCC name {
      LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
      LANG = "C.UTF-8";
    } ''
      echo "Running test suite ${name}"

      ${impl.runScript} ${
        impl.genTestLisp {
          inherit name deps expression;
          srcs = srcsForImpl;
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
  # - bundled :: { ${implementation}, ... } -> library
  #   Allows giving a implementation specific builder for a bundled library.
  #   This function is used as a replacement for the default internal bundled'
  #   function and must only support one implementation. The returned derivation
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
        let lispDeps = filter (d: !d.lispBinary) (allDeps deps);
        in writeShellScriptBin "sbcl" ''
          export LD_LIBRARY_PATH="${lib.makeLibraryPath (allNative [] lispDeps)}"
          export LANG="C.UTF-8"
          exec ${sbcl}/bin/sbcl ${
            lib.optionalString (deps != [])
              "--load ${writeText "load.lisp" (impls.sbcl.genLoadLisp lispDeps)}"
          } $@
        '';
    };
    ecl = {
      runScript = "${ecl-static}/bin/ecl --shell";
      faslExt = "fasc";
      genLoadLisp = genLoadLispGeneric impls.ecl;
      genCompileLisp = { name, srcs, deps }: writeText "ecl-compile.lisp" ''
        ;; We need to do this here, so reading the let* later doesn't fail
        ;; when it doesn't find the c:build-static-library symbol for some
        ;; reason. Not sure if there's a better workaround for this.
        (ext:install-c-compiler)

        ${impls.ecl.genLoadLisp deps}

        (defun getenv-or-fail (var)
          (or (ext:getenv var)
              (error (format nil "Missing expected environment variable ~A" var))))

        (defun nix-compile-file (native srcfile)
          (let* ((nix-build-top (getenv-or-fail "NIX_BUILD_TOP"))
                 (unique-name (substitute #\_ #\/ srcfile))
                 (out-file (make-pathname :type (if native "o" "fasc")
                                          :directory nix-build-top
                                          :name unique-name)))
            (multiple-value-bind (out-truename _warnings-p failure-p)
                (compile-file srcfile :system-p native
                                      :load (not native)
                                      :output-file out-file)
              (if failure-p (ext:quit 1) out-truename))))

        (let* ((*compile-verbose* t)
               (*compile-print* t)
               (srcs
                ;; These forms are inserted by the Nix build
                '(${lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs})))

          (ext:install-bytecodes-compiler)

          ;; Build a byte-compiled FASL (.fasc) file. This can be loaded
          ;; dynamically via 'load'. It is slower than the natively compiled
          ;; equivalent (.fas which is a shared object internally), but we only
          ;; use this while compiling and in lispWith where performance is not
          ;; as critical. The built libraries and executables use the static
          ;; archive.
          (with-open-file (output (make-pathname :type "fasc" :name "${name}"
                                  :directory (getenv-or-fail "out"))
                           :direction :output
                           :if-does-not-exist :create)

            (ext:run-program "cat" (mapcar (lambda (x) (namestring (nix-compile-file nil x))) srcs) :output output))

          (ext:install-c-compiler)

          ;; Build a (natively compiled) static archive (.a) file. This is
          ;; necessary for linking an executable later.
          (c:build-static-library
           (make-pathname :type "a" :name "${name}"
                          :directory (getenv-or-fail "out"))
           :lisp-files (mapcar (lambda (x) (nix-compile-file t x)) srcs)))
      '';
      genDumpLisp = { name, main, deps }: writeText "ecl-dump.lisp" ''
        (defun getenv-or-fail (var)
          (or (ext:getenv var)
              (error (format nil "Missing expected environment variable ~A" var))))

        ${impls.ecl.genLoadLisp deps}

        ;; makes a 'c' package available that can link executables
        (ext:install-c-compiler)

        (c:build-program
         (make-pathname :name "${name}"
                        :directory (concatenate 'string
                                                (getenv-or-fail "out")
                                                "/bin"))
         :epilogue-code '(progn (${main}) (ext:quit))
         :ld-flags '("-static")
         :lisp-files
         ;; The following forms are inserted by the Nix build
         '(${
             let
               depsWithImpl = builtins.map (
                 dep: dep.overrideLisp (_: { implementation = "ecl"; })
               ) (allDeps deps);
             in
               lib.concatMapStrings (dep: ''
                 "${dep}/${dep.lispName}.a"
               '') depsWithImpl
           }))
      '';
      genTestLisp = genTestLispGeneric impls.ecl;

      lispWith = deps:
        let lispDeps = filter (d: !d.lispBinary) (allDepsForImpl impls.ecl deps);
        in writeShellScriptBin "ecl" ''
          exec ${ecl-static}/bin/ecl ${
            lib.optionalString (deps != [])
              "--load ${writeText "load.lisp" (impls.ecl.genLoadLisp lispDeps)}"
          } $@
        '';

      bundled = name: runCommandNoCC "${name}-cllib" {
        passthru = {
          lispName = name;
          lispNativeDeps = [];
          lispDeps = [];
          lispBinary = false;
          ecl = impls.ecl.lispWith [ (impls.ecl.bundled name) ];
        };
      } ''
        mkdir -p "$out"
        ln -s "${ecl-static}/lib/ecl-${ecl-static.version}/${name}.${impls.ecl.faslExt}" -t "$out"
        ln -s "${ecl-static}/lib/ecl-${ecl-static.version}/lib${name}.a" "$out/${name}.a"
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
      srcsForImpl = filterSrcs impl srcs;
      lispNativeDeps = (allNative native deps);
      lispDeps = allDeps deps;
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs = srcsForImpl ++ (tests.srcs or []);
          deps = deps ++ (tests.deps or []);
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
        "${impl.name}" = impl.lispWith [ self ];
      };
    } ''
      ${if ! isNull testDrv
        then "echo 'Test ${testDrv} succeeded'"
        else "echo 'No tests run'"}

      mkdir $out

      ${impl.runScript} ${
        impl.genCompileLisp {
          srcs = srcsForImpl;
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
      lispDeps = allDeps deps;
      libPath = lib.makeLibraryPath (allNative native lispDeps);
      srcsForImpl = filterSrcs impl srcs;
      # overriding is used internally to propagate the implementation to use
      selfLib = (makeOverridable library) {
        inherit name native;
        deps = lispDeps;
        srcs = srcsForImpl;
      };
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs =
            ( # testSuite does run filterSrcs as well
              srcsForImpl ++ (tests.srcs or []));
          deps = deps ++ (tests.deps or []);
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
        lispDeps = [ selfLib ] ++ (tests.deps or []);
        lispNativeDeps = native;
        lispBinary = true;
        tests = testDrv;
        sbcl = impl.lispWith [ self ];
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
  # by calling 'require', but implementations are free to provide a more
  # their own specific bundled function via impls."${implementation}".bundled.
  #
  # Example usage:
  #
  # bundled {
  #   ecl = "ext";
  #   sbcl = "sb-posix";
  #   default = "uiop";
  # }
  #
  # bundled "asdf" # equivalent to bundled { default = "asdf"; }
  bundled = arg:
    let
      emptyLib = implementation: library {
        name = "empty";
        srcs = [
          (builtins.toFile "empty.lisp" "nil")
        ];
        inherit implementation;
      };

      bundled' =
        { # the implementation to _actually_ build with
          implementation ? defaultImplementation
        , ...
        }@args:

        let
          name = args."${implementation}" or args.default or
            (builtins.throw "Bundled dependency not available for ${implementation}");

          defaultBundled = name: library {
            inherit name;
            srcs = lib.singleton (builtins.toFile "${name}.lisp" "(require '${name})");
          };

        in
          if name == null
          then emptyLib implementation
          else impls."${implementation}".bundled or defaultBundled name;

      args =
        if builtins.isString arg
        then { default = arg; }
        else arg;
    in (makeOverridable bundled') args;
in {
  library = makeOverridable library;
  program = makeOverridable program;
  inherit bundled;

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded in SBCL.
  sbclWith = deps: impls.sbcl.lispWith (
    builtins.map (dep: dep.overrideLisp (_: { implementation = "sbcl"; }))
  );
}
