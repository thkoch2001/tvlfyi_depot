with import <nixpkgs> {};
with python35Packages;

buildPythonPackage {
  name = "wpcarro";
  src = ./day_5.py;
  propagatedBuildInputs = [ pytest numpy ];
}
