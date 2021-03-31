{ depot, pkgs, ... }:

let
  inherit (pkgs) graphviz runCommandNoCC writeText;

  tvlGraph = runCommandNoCC "tvl.svg" {
    nativeBuildInputs = with pkgs; [ fontconfig freetype cairo jetbrains-mono ];
  } ''
    ${graphviz}/bin/neato -Tsvg ${./tvl.dot} > $out
  '';

  homepage = writeText "index.html" ''
    <!DOCTYPE html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <meta name="description" content="The Virus Lounge">
      <link rel="stylesheet" type="text/css" href="/static/tazjin.css" media="all">
      <link rel="icon" type="image/webp" href="/static/favicon.webp">
      <title>The Virus Lounge</title>
      <style>
        svg {
          max-width: inherit;
          height: auto;
        }
      </style>
    </head>
    <body class="light">
      <header>
        <h1><a class="blog-title" href="/">The Virus Lounge</a> </h1>
        <hr>
      </header>

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

      <hr>
      <footer>
        <p class="footer">
          <a class="uncoloured-link" href="https://tazj.in">homepage</a>
          |
          <a class="uncoloured-link" href="https://cs.tvl.fyi/depot/-/blob/README.md">code</a>
          |
          <a class="uncoloured-link" href="https://twitter.com/tazjin">twitter</a>
        </p>
        <p class="lod">ಠ_ಠ</p>
      </footer>
    </body>
  '';
in runCommandNoCC "website" {} ''
  mkdir -p $out/static
  cp ${homepage} $out/index.html
  cp -r ${./static}/* $out/static

  # Some assets are stolen from tazjin's blog
  cp ${depot.users.tazjin.homepage}/static/jetbrains-* $out/static
  cp ${depot.users.tazjin.homepage}/static/tazjin.css $out/static
''
