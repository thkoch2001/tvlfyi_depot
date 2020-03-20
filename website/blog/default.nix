{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "blog.wpcarro.dev";
  buildInputs = with pkgs; [ hugo ];
  src = ./.;
  buildPhase = ''
    mkdir -p $out
    ${pkgs.hugo}/bin/hugo --minify --destination $out
  '';
  dontInstall = true;
}
