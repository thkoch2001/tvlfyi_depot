{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "habits-webpage";
  src = builtins.path { path = ../../playbooks; name = "playbooks"; };
  buildInputs = [];
  buildPhase = ''
    ${pkgs.pandoc}/bin/pandoc $src/habits.org -o index.html
  '';
  installPhase = ''
    mv index.html $out
  '';
}
