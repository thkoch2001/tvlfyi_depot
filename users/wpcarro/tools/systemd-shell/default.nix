{ pkgs, ... }:

pkgs.python310Packages.buildPythonApplication {
  pname = "systemd-shell";
  version = "0.0.1";
  src = ./.;
  doCheck = false;
}
