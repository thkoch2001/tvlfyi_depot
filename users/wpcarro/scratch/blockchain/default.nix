{ pkgs
, ...
}:
let
  pypkgs = pkgs.python3Packages;
in
pkgs.python3Packages.buildPythonApplication
  { pname = "main"; src = ./.; version = "0.0.1"; propagatedBuildInputs = with pypkgs; [ flask requests ]; }
