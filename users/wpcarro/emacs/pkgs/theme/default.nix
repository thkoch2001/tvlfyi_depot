{ pkgs, depot, ... }:

pkgs.callPackage
  ({ emacsPackages }:
  emacsPackages.trivialBuild {
    pname = "theme";
    version = "1.0.0";
    src = ./theme.el;
    packageRequires =
      (with depot.users.wpcarro.emacs.pkgs; [
        cycle
      ]);
  })
{ }
