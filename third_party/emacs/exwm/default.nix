# EXWM is present in nixpkgs and we do not (currently) intend to
# change the code structure, so the existing drv can be reused.
{ pkgs, lib, ... }:

let
  inherit (pkgs.emacsPackages) melpaBuild xelb;
in melpaBuild {
  pname = "exwm";
  ename = "exwm";
  version = "0.24";
  src = ./.;
  packageRequires = [ xelb ];

  recipe = builtins.toFile "recipe.el" ''
    (exwm :fetcher github :repo "ch11ng/exwm")
  '';

  meta = {
    homepage = "https://elpa.gnu.org/packages/exwm.html";
    license = lib.licenses.free;
  };
}
