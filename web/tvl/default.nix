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
      The Virus Lounge
      ================

      ----------------

      <img class="tvl-logo" src="/static/tvl-animated.svg"
           alt="Virus with lambda-shaped spike proteins sitting on an armchair">

      Welcome to **The Virus Lounge**. We're a random group of people
      who feel undersocialised in these trying times, and we've
      decided that there isn't enough spontaneous socialising on the
      internet.

      <hr>

      ## Where did all these people come from?

      It's pretty straightforward. Feel free to click on people, too.

      <div class="tvl-graph-container">
        <!--
          cheddar leaves HTML inside of HTML alone,
          so wrapping the SVG prevents it from messing it up
        -->
        ${builtins.readFile tvlGraph}
      </div>
    '';
    extraHead = ''
      <style>
        .tvl-graph-container {
          max-width: inherit;
        }

        .tvl-graph-container svg {
          max-width: inherit;
          height: auto;
        }

        .tvl-logo {
          width: 60%;
          display: block;
          margin-left: auto;
          margin-right: auto;
        }
      </style>
    '';
  };
in runCommandNoCC "website" {} ''
  mkdir -p $out/static
  cp ${homepage} $out/index.html
  cp ${depot.web.static}/* $out/static
  cp ${depot.web.tvl.logo.pastelRainbow} $out/static/tvl-animated.svg
''
