{  pkgs, ... }:

let
  inherit (pkgs) lib;
  src = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/aaptel/notmuch-extract-patch/f732a53e12a7c91a06755ebfab2007adc9b3063b/notmuch-extract-patch";
    sha256 = "0nawkl04sj7psw6ikzay7kydj3dhd0fkwghcsf5rzaw4bmp4kbax";
  };

in pkgs.runCommandNoCC "notmuch-extract-patch" {
  buildInputs = [ pkgs.python3 ];
} ''
  mkdir -p $out/bin
  install -m 755 ${src} $out/bin/notmuch-extract-patch
  patchShebangs $out/bin
''
