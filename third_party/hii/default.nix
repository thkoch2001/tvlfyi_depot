{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  pname = "hii";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "nmeum";
    repo = "hii";
    rev = "f3fe972025f1e904c119d0ca16b144d4729749aa";
    sha256 = "17k07hps6ywr0f1rmls1jddv0rxv7zfnanck6427v0kkxpblprwr";
    fetchSubmodules = true;
  };

  outputs = [ "bin" "man" "doc" "out" ];
  PREFIX = placeholder "out";
  BINDIR = "${placeholder "bin"}/bin";
  MANDIR = "${placeholder "man"}/share/man";
  DOCDIR = "${placeholder "doc"}/share/doc/hii";

  preBuild = ''
    export HOME="$(mktemp -d)"
  '';

  nativeBuildInputs = [
    pkgs.go
  ];
}
