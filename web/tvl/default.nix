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
      <main>
        <img alt="The Virus Lounge" src="/static/virus_lounge.webp">
      </main>

      <p>
        Welcome to <b>The Virus Lounge</b>. We're a random group of
        people who feel undersocialised in these trying times, and
        we've decided that there isn't enough spontaneous socialising
        on the internet.
      </p>

      <hr>
      <h2>Where did all these people come from?</h2>

      <p>
        It's pretty straightforward. Feel free to click on people, too.
      </p>
      ${builtins.readFile tvlGraph}
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
  mkdir $out
  cp ${homepage} $out/index.html
  cp -r ${depot.web.static} $out/static
''
