{ pkgs, depot, ... }:

let
  struct = pkgs.callPackage
    ({ emacsPackages }:
      emacsPackages.trivialBuild {
        pname = "struct";
        version = "1.0.0";
        src = ./struct.el;
        packageRequires =
          (with emacsPackages; [
            s
          ]);
      })
    { };

  emacs = (pkgs.emacsPackagesFor pkgs.emacs28).emacsWithPackages (epkgs: [
    struct
  ]);
in
struct.overrideAttrs (_old: {
  doCheck = true;
  checkPhase = ''
    ${emacs}/bin/emacs -batch \
      -l ert -l ${./tests.el} -f ert-run-tests-batch-and-exit
  '';
})
