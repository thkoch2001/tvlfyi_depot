{ pkgs, depot, ... }:

pkgs.callPackage
  ({ emacsPackages }:
  emacsPackages.trivialBuild {
    pname = "vterm-mgt";
    version = "1.0.0";
    src = ./vterm-mgt.el;
    packageRequires =
      (with emacsPackages; [
        dash
        vterm
      ]) ++
      (with depot.users.wpcarro.emacs.pkgs; [
        cycle
      ]);
  })
{ }
