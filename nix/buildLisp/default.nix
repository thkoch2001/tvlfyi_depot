# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  inherit (builtins) map elemAt match filter;
  inherit (pkgs) lib runCommandNoCC makeWrapper writeText writeShellScriptBin sbcl ecl;

  #
  # Internal helper definitions
  #

  genLoadLispGeneric = impl: deps:
    let
      depsWithImpl = builtins.map (
        dep: dep.overrideLisp (_: { implementation = impl.name; })
      ) (allDeps deps);
    in lib.concatStringsSep "\n"
      (map (lib: "(load \"${lib}/${lib.lispName}.${impl.faslExt}\")") depsWithImpl);

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
    in runCommandNoCC name {
      LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
      LANG = "C.UTF-8";
    } ''
      echo "Running test suite ${name}"

      ${impl.runScript} ${
        impl.genTestLisp {
          inherit name srcs deps expression;
        }
      } | tee $out

      echo "Test suite ${name} succeeded"
    '';

  impls = lib.mapAttrs (name: v: { inherit name; } // v) {
    sbcl = {
      runScript = "${sbcl}/bin/sbcl --script";
      faslExt = "fasl";

      # 'genLoadLisp' generates Lisp code that instructs SBCL to load all
      # the provided Lisp libraries.
      genLoadLisp = genLoadLispGeneric impls.sbcl;

      # 'genCompileLisp' generates a Lisp file that instructs SBCL to
      # compile the provided list of Lisp source files to $out.
      genCompileLisp = { name, srcs, deps }: writeText "sbcl-compile.lisp" ''
        ;; This file compiles the specified sources into the Nix build
        ;; directory, creating one FASL file for each source.
        (require 'sb-posix)

        ${impls.sbcl.genLoadLisp deps}

        (defun pipe (in-stream out-stream)
          (loop for u = (read-byte in-stream nil nil)
                while u do (write-byte u out-stream)))

        (defun nix-compile-lisp (file srcfile)
          (let ((outfile (make-pathname :type "fasl"
                                        :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                       (error "not running in a Nix build"))
                                        :name (substitute #\- #\/ srcfile))))
            (multiple-value-bind (_outfile _warnings-p failure-p)
                (compile-file srcfile :output-file outfile)
              (if failure-p (sb-posix:exit 1)
                  (progn
                    ;; For the case of multiple files belonging to the same
                    ;; library being compiled, load them in order:
                    (load outfile)

                    ;; Write them to the FASL list in the same order:
                    (with-open-file (compiled-file outfile
                                                   :direction :input
                                                   :element-type 'unsigned-byte)
                      (pipe compiled-file file)))))))

        (let ((*compile-verbose* t)
              (catted-fasl (make-pathname :type "fasl"
                                          :directory (or (sb-posix:getenv "out")
                                                         (error "not running in a Nix build"))
                                          :name "${name}")))

          (with-open-file (file catted-fasl
                                :direction :output
                                :if-does-not-exist :create
                                :element-type 'unsigned-byte)

            ;; These forms were inserted by the Nix build:
            ${
              lib.concatStringsSep "\n" (map (src: "(nix-compile-lisp file \"${src}\")") srcs)
            }
            ))
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
        ;;
      '';

      genTestLisp = genTestLispGeneric impls.sbcl;

      lispWith = sbclWith;
    };
    ecl = {
      runScript = "${ecl}/bin/ecl -shell";
      faslExt = "fas";
      genLoadLisp = genLoadLispGeneric impls.ecl;
      genCompileLisp = { name, srcs, deps }: writeText "ecl-compile.lisp" ''
        ;; makes ECL use a C compiler (and not its byte compiler)
        ;; and makes the 'c' package available.
        (ext:install-c-compiler)

        ${impls.ecl.genLoadLisp deps}

        (defun getenv-or-fail (var)
          (or (ext:getenv var)
              (error (format nil "Missing expected environment variable ~A" var))))

        (defun nix-compile-file (srcfile)
          (let* ((nix-build-top (getenv-or-fail "NIX_BUILD_TOP"))
                 (unique-name (substitute #\_ #\/ srcfile))
                 (out-file (make-pathname :type "o" :directory nix-build-top
                                          :name unique-name)))
            (multiple-value-bind (out-truename _warnings-p failure-p)
                (compile-file srcfile :system-p t :output-file out-file)
              (if failure-p (ext:quit 1)
                  (progn
                    ;; let's load fasl built from the same object file as the
                    ;; final fasl to prevent any discrepancies.
                    (let ((tmp-fasl (make-pathname :type "fas" :name unique-name
                                                   :directory nix-build-top)))
                      (c:build-fasl tmp-fasl :lisp-files (list out-truename))
                      (load tmp-fasl))
                    ;; finally return the pathname of the built object
                    out-truename)))))

        (let* ((srcs
                ;; These forms are inserted by the Nix build
                '(${lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs}))
               (objs (mapcar #'nix-compile-file srcs)))

          ;; TODO(sterni): build static lib
          ;; Build a (natively compiled) FASL (.fas) file. This can be loaded
          ;; dynamically via 'load' and is a shared object file internally
          (c:build-fasl
           (make-pathname :type "fas" :name "${name}"
                          :directory (getenv-or-fail "out"))
           :lisp-files objs)

          ;; Build a (natively compiled) static archive (.a) file. This is
          ;; necessary for linking an executable later.
          (c:build-static-library
           (make-pathname :type "a" :name "${name}"
                          :directory (getenv-or-fail "out"))
           :lisp-files objs))
      '';
      genDumpLisp = { name, main, deps }: writeText "ecl-dump.lisp" ''
        ;; makes ECL use a C compiler (and not its byte compiler)
        ;; and makes the 'c' package available.
        (ext:install-c-compiler)

        (defun getenv-or-fail (var)
          (or (ext:getenv var)
              (error (format nil "Missing expected environment variable ~A" var))))

        ${impls.ecl.genLoadLisp deps}

        (c:build-program
         (make-pathname :name "${name}"
                        :directory (concatenate 'string
                                                (getenv-or-fail "out")
                                                "/bin"))
         :epilogue-code '(progn (${main}) (ext:quit))
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
      lispWith = _: pkgs.emptyFile;
    };
  };

  #
  # Public API functions
  #

  # 'library' builds a list of Common Lisp files into a single FASL
  # which can then be loaded into SBCL.
  library =
    { name
    , implementation ? "sbcl"
    , srcs
    , deps ? []
    , native ? []
    , tests ? null
    }:
    let
      impl = impls."${implementation}" or
        (builtins.throw "Unkown Common Lisp Implementation ${implementation}");
      lispNativeDeps = (allNative native deps);
      lispDeps = allDeps deps;
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs = srcs ++ (tests.srcs or []);
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
          inherit name srcs;
          deps = lispDeps;
        }
      }
    '');

  # 'program' creates an executable containing a dumped image of the
  # specified sources and dependencies.
  program =
    { name
    , implementation ? "sbcl"
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
      # overriding is used internally to propagate the implementation to use
      selfLib = (makeOverridable library) {
        inherit name srcs native;
        deps = lispDeps;
      };
      testDrv = if ! isNull tests
        then testSuite {
          name = tests.name or "${name}-test";
          srcs =
            (
              srcs ++ (tests.srcs or []));
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
        sbcl = sbclWith [ self ];
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

  # 'bundled' creates a "library" that calls 'require' on a built-in
  # package, such as any of SBCL's sb-* packages.
  bundled = name: (makeOverridable library) {
    inherit name;
    srcs = lib.singleton (builtins.toFile "${name}.lisp" "(require '${name})");
  };

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded.
  sbclWith = deps:
  let lispDeps = filter (d: !d.lispBinary) (allDeps deps);
  in writeShellScriptBin "sbcl" ''
    export LD_LIBRARY_PATH="${lib.makeLibraryPath (allNative [] lispDeps)}"
    export LANG="C.UTF-8"
    exec ${sbcl}/bin/sbcl ${lib.optionalString (deps != []) "--load ${writeText "load.lisp" (impls.sbcl.genLoadLisp lispDeps)}"} $@
  '';
in {
  library = makeOverridable library;
  program = makeOverridable program;
  inherit sbclWith bundled;
}
