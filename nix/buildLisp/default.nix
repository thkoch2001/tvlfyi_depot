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
      runScript = "${ecl-static}/bin/ecl -shell";
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

        (let* ((srcs
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
                           :direction :output)
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
      lispWith = _: pkgs.emptyFile;

      bundled = name: runCommandNoCC "${name}-cllib" {
        passthru = {
          lispName = name;
          lispNativeDeps = [];
          lispDeps = [];
          lispBinary = false;
          eclWith = impls.ecl.lispWith [ (impls.ecl.bundled name) ];
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

  # 'library' builds a list of Common Lisp files into a single FASL
  # which can then be loaded into SBCL.
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
  bundled = arg:
    let
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

       in impls."${implementation}".bundled or defaultBundled name;
    in (makeOverridable bundled')
      (if builtins.isString arg then { default = arg; } else arg);

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
