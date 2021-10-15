# dfmt is a code formatter for D
{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "dfmt";
  version = "0.13.4";

  src = pkgs.fetchFromGitHub {
    owner = "dlang-community";
    repo = "dfmt";
    rev = "v${version}";
    sha256 = "02a8qvrmnl1c15y6irxlhqpr0hjj5s8qk0jc20ivj0fl6p4v9shj";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [
    dmd

    # fake git that will be used to fetch the version string
    (pkgs.writeShellScriptBin "git" "echo 'v${version}'")
  ];

  DFLAGS = "-release";

  installPhase = ''
    mkdir -p $out/bin
    cp bin/dfmt $out/bin/
    strip $out/bin/dfmt
  '';

  meta = {
    description = "D code formatter";
    homepage = "https://github.com/dlang-community/dfmt";
    license = lib.licenses.boost;
  };
}
