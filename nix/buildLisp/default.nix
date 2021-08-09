# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  inherit (builtins) map elemAt match filter;
  inherit (pkgs) lib runCommandNoCC makeWrapper writeText writeShellScriptBin sbcl ecl-static;

  #
  # Internal helper definitions
  #

  defaultImplementation = "sbcl";

  disableDebugger = writeText "disable-debugger.lisp" ''
    (setf *debugger-hook*
          (lambda (error hook)
            (declare (ignore hook))
            (format *error-output* "~%Unhandled error: ~a~%" error)
            #+ecl (ext:quit 1)))
  '';

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
    # would only expose its actually used dependencies. Use implementation
    # attribute created by withExtras if present, override in all other cases
    # (mainly bundled).
    deps' = builtins.map (dep: dep."${impl.name}" or (dep.overrideLisp (_: {
      implementation = impl.name;
    }))) deps;
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

  # This is a wrapper arround 'makeOverridable' which performs its
  # function, but also adds a the following additional attributes to the
  # resulting derivation, namely a repl attribute which builds a `lispWith`
  # derivation for the current implementation and additional attributes for
  # every all implementations. So `drv.sbcl` would build the derivation
  # with SBCL regardless of what was specified in the initial arguments.
  withExtras = f: args:
    let
      drv = (makeOverridable f) args;
    in lib.fix (self:
      drv.overrideLisp (old:
        let
          implementation = old.implementation or defaultImplementation;
          badImplementations = old.badImplementations or [];
          targets = lib.subtractLists badImplementations
            (builtins.attrNames impls);
        in {
          passthru = (old.passthru or {}) // {
            repl = impls."${implementation}".lispWith [ self ];

            # meta is done via passthru to minimize rebuilds caused by overriding
            meta = (old.passthru.meta or {}) // {
              inherit targets;
            };
          } // builtins.listToAttrs (builtins.map (name: {
            inherit name;
            value = self.overrideLisp (_: {
              implementation = name;
            });
          }) (builtins.attrNames impls));
        }) // {
          overrideLisp = new: withExtras f (args // new args);
        });

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
  # - wrapProgram :: boolean
  #   Whether to wrap the resulting binary / image with a wrapper script setting
  #   `LD_LIBRARY_PATH`.
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

      wrapProgram = true;

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
    ecl = {
      runScript = "${ecl-static}/bin/ecl --load ${disableDebugger} --shell";
      faslExt = "fasc";
      genLoadLisp = genLoadLispGeneric impls.ecl;
      genCompileLisp = { name, srcs, deps }: writeText "ecl-compile.lisp" ''
        ;; This seems to be required to bring make the 'c' package available
        ;; early, otherwise ECL tends to fail with a read failure…
        (ext:install-c-compiler)

        ;; Load dependencies
        ${impls.ecl.genLoadLisp deps}

        (defun getenv-or-fail (var)
          (or (ext:getenv var)
              (error (format nil "Missing expected environment variable ~A" var))))

        (defun nix-compile-file (srcfile &key native)
          "Compile the given srcfile into a compilation unit in :out-dir using
          a unique name based on srcfile as the filename which is returned after
          compilation. If :native is true, create an native object file,
          otherwise a byte-compile fasc file is built and immediately loaded."

          (let* ((unique-name (substitute #\_ #\/ srcfile))
                 (out-file (make-pathname :type (if native "o" "fasc")
                                          :directory (getenv-or-fail "NIX_BUILD_TOP")
                                          :name unique-name)))
            (multiple-value-bind (out-truename _warnings-p failure-p)
                (compile-file srcfile :system-p native
                                      :load (not native)
                                      :output-file out-file
                                      :verbose t :print t)
              (if failure-p (ext:quit 1) out-truename))))

        (let* ((out-dir (getenv-or-fail "out"))
               (nix-build-dir (getenv-or-fail "NIX_BUILD_TOP"))
               (srcs
                ;; These forms are inserted by the Nix build
                '(${lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs})))

          ;; First, we'll byte compile loadable FASL files and load them
          ;; immediately. Since we are using a statically linked ECL, there's
          ;; no way to load native objects, so we rely on byte compilation
          ;; for all our loading — which is crucial in compilation of course.
          (ext:install-bytecodes-compiler)

          ;; ECL's bytecode FASLs can just be concatenated to create a bundle
          ;; at least since a recent bugfix which we apply as a patch.
          ;; See also: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/649
          (let ((bundle-out (make-pathname :type "fasc" :name "${name}"
                                           :directory out-dir)))

            (with-open-file (fasc-stream bundle-out :direction :output)
              (ext:run-program "cat"
                               (mapcar (lambda (f)
                                         (namestring
                                          (nix-compile-file f :native nil)))
                                       srcs)
                               :output fasc-stream)))

          (ext:install-c-compiler)

          ;; Build a (natively compiled) static archive (.a) file. We want to
          ;; use this for (statically) linking an executable later. The bytecode
          ;; dance is only required because we can't load such archives.
          (c:build-static-library
           (make-pathname :type "a" :name "${name}" :directory out-dir)
           :lisp-files (mapcar (lambda (x)
                                 (nix-compile-file x :native t))
                               srcs)))
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
         :epilogue-code `(progn
                          ;; UIOP doesn't understand ECL, so we need to make it
                          ;; aware that we are a proper executable, causing it
                          ;; to handle argument parsing and such properly. Since
                          ;; this needs to work even when we're not using UIOP,
                          ;; we need to do some compile-time acrobatics.
                          ,(when (find-package 'uiop)
                            `(setf ,(find-symbol "*IMAGE-DUMPED-P*" :uiop) :executable))
                          ;; Run the actual application…
                          (${main})
                          ;; … and exit.
                          (ext:quit))
         ;; ECL can't remember these from its own build…
         :ld-flags '("-static")
         :lisp-files
         ;; The following forms are inserted by the Nix build
         '(${
             lib.concatMapStrings (dep: ''
               "${dep}/${dep.lispName}.a"
             '') (allDeps impls.ecl deps)
           }))
      '';

      wrapProgram = false;

      genTestLisp = genTestLispGeneric impls.ecl;

      lispWith = deps:
        let lispDeps = filter (d: !d.lispBinary) (allDeps impls.ecl deps);
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
          repl = impls.ecl.lispWith [ (impls.ecl.bundled name) ];
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
    , badImplementations ? [] # TODO(sterni): make this a warning
    , srcs
    , deps ? []
    , native ? []
    , tests ? null
    , passthru ? {}
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
      passthru = passthru // {
        inherit lispNativeDeps lispDeps;
        lispName = name;
        lispBinary = false;
        tests = testDrv;
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
    , badImplementations ? [] # TODO(sterni): make this a warning
    , main ? "${name}:main"
    , srcs
    , deps ? []
    , native ? []
    , tests ? null
    , passthru ? {}
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
        inherit name native badImplementations;
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
      passthru = passthru // {
        lispName = name;
        lispDeps = [ selfLib ];
        lispNativeDeps = native;
        lispBinary = true;
        tests = testDrv;
      };
    } (''
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
    '' + lib.optionalString impl.wrapProgram ''
      wrapProgram $out/bin/${name} --prefix LD_LIBRARY_PATH : "${libPath}"
    ''));

  # 'bundled' creates a "library" which makes a built-in package available,
  # such as any of SBCL's sb-* packages or ASDF. By default this is done
  # by calling 'require', but implementations are free to provide their
  # own specific bundled function.
  bundled = name:
    let
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
  library = withExtras library;
  program = withExtras program;
  inherit bundled;

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded in SBCL.
  sbclWith = deps: impls.sbcl.lispWith deps;
}
