{ pkgs, depot, ... }:

let
  list = pkgs.callPackage
    ({ emacsPackages }:
      emacsPackages.trivialBuild {
        pname = "list";
        version = "1.0.0";
        src = ./list.el;
        packageRequires =
          (with emacsPackages; [
            dash
          ]) ++
          (with depot.users.wpcarro.emacs.pkgs; [
            maybe
            set
          ]);
      })
    { };

  emacs = (pkgs.emacsPackagesFor pkgs.emacs28).emacsWithPackages (epkgs: [ list ]);
in
list.overrideAttrs (_old: {
  doCheck = true;
  checkPhase = ''
    ${emacs}/bin/emacs -batch \
      -l ert -l ${./tests.el} -f ert-run-tests-batch-and-exit
  '';
})
