{ depot, pkgs, ... }:

let
  inherit (pkgs) graphviz runCommandNoCC writeText;

  tvlGraph = runCommandNoCC "tvl.svg" {
    nativeBuildInputs = with pkgs; [ fontconfig freetype cairo jetbrains-mono ];
  } ''
    ${graphviz}/bin/neato -Tsvg ${./tvl.dot} > $out
  '';

  homepage = depot.web.tvl.template {
    title = "The Virus Lounge";
    content = ''
      ![The Virus Lounge](/static/virus_lounge.webp)

      Welcome to **The Virus Lounge**. We're a random group of
      people who feel undersocialised in these trying times, and
      we've decided that there isn't enough spontaneous socialising
      on the internet.

      <hr>

      ## Where did all these people come from?

      It's pretty straightforward. Feel free to click on people, too.

      ![The TVL Graph](/static/tvl.svg)
    '';
    extraHead = ''
      <style>
        svg {
          max-width: inherit;
          height: auto;
        }
      </style>
    '';
  };
in runCommandNoCC "website" {} ''
  mkdir -p $out/static
  cp ${homepage} $out/index.html
  cp -r ${./static}/* $out/static
  cp ${tvlGraph} $out/static/tvl.svg

  # Some assets are stolen from tazjin's blog
  cp ${depot.users.tazjin.homepage}/static/jetbrains-* $out/static
  cp ${depot.users.tazjin.homepage}/static/tazjin.css $out/static
''
