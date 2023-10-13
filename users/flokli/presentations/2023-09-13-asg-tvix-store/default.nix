{ depot, pkgs, ... }:

let
  inherit (pkgs)
    fontconfig qrencode runCommand stdenv;
  mkQr = url: runCommand "qrcode.png" { } ''
    ${qrencode}/bin/qrencode -o $out -t SVG -s 5 \
      --background=fafafa \
      --foreground=000000 \
      ${url}
  '';
in
stdenv.mkDerivation {
  name = "2023-asg-tvix-store";
  src = ./.;

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = with pkgs; [ jetbrains-mono fira fira-code fira-mono lato ];
  };

  nativeBuildInputs = [ pkgs.reveal-md pkgs.graphviz ];

  buildPhase = ''
    cp ${depot.tvix.logo}/logo.png tvix-logo.png
    cp ${mkQr "https://flokli.de"} qrcode-flokli.svg
    cp ${mkQr "https://tvix.dev"} qrcode-tvix.svg

    mkdir -p $out
    cp tvix-store-graph-blob-directory.svg $out/
    reveal-md --static $out presentation.md
  '';
}
