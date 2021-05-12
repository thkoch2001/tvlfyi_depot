{ depot, pkgs, ... }:

{ # content of the <title> tag
  title
  # main part of the page, usually wrapped with <main>
, content
  # optional extra html to inject into <head>
, extraHead ? null
  # whether to use global URLs instead of absolute paths
, useUrls ? false
}@args:

let
  inherit (pkgs) writeText lib;

  baseUrl = lib.optionalString useUrls "https://tvl.fyi";
in

writeText "index.html" (''
  <!DOCTYPE html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="The Virus Lounge">
    <link rel="stylesheet" type="text/css" href="${baseUrl}/static/tazjin.css" media="all">
    <link rel="icon" type="image/webp" href="${baseUrl}/static/favicon.webp">
    <title>${title}</title>
'' + lib.optionalString (args ? extraHead) extraHead + ''
  </head>
  <body class="light">
    <header>
      <h1><a class="blog-title" href="/">${title}</a> </h1>
      <hr>
    </header>

    ${content}

    <hr>
    <footer>
      <p class="footer">
        <a class="uncoloured-link" href="https://cs.tvl.fyi/depot/-/blob/README.md">code</a>
        |
        <a class="uncoloured-link" href="https://cl.tvl.fyi/">reviews</a>
        |
        <a class="uncoloured-link" href="https://b.tvl.fyi/">bugs</a>
        |
        <a class="uncoloured-link" href="https://todo.tvl.fyi/">todos</a>
        |
        <a class="uncoloured-link" href="https://atward.tvl.fyi/">atward</a>
      </p>
      <p class="lod">ಠ_ಠ</p>
    </footer>
  </body>
'')
