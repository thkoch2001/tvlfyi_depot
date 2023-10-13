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
  name = "2023-nixcon-tvix";
  src = ./.;

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = with pkgs; [ jetbrains-mono fira fira-code fira-mono lato ];
  };

  PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
  PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = "1";

  nativeBuildInputs = [ pkgs.reveal-md pkgs.graphviz ];

  buildPhase = ''
    cp ${depot.tvix.logo}/logo.png tvix-logo.png
    dot -Tsvg crate-deps.dot > crate-deps.svg
    cp ${mkQr "https://flokli.de"} qrcode-flokli.svg
    cp ${mkQr "https://tvix.dev"} qrcode-tvix.svg

    mkdir -p $out
    reveal-md --static $out presentation.md
    reveal-md --print $out/slides.pdf presentation.md
    cp tvixbolt.webm $out
  '';
}
