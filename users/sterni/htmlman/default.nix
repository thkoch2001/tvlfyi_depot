{ depot, lib, pkgs, ... }:

let
  inherit (depot.nix)
    getBins
    runExecline
    ;

  inherit (pkgs)
    lowdown
    mandoc
    coreutils
    fetchurl
    ;

  bins = (getBins lowdown [ "lowdown" ])
      // (getBins mandoc [ "mandoc" ])
      // (getBins coreutils [ "cat" "mv" "mkdir" ])
      ;

  normalizeDrv = fetchurl {
    url = "https://necolas.github.io/normalize.css/8.0.1/normalize.css";
    sha256 = "04jmvybwh2ks4dlnfa70sb3a3z3ig4cv0ya9rizjvm140xq1h22q";
  };

  stdout = "1";

  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  markdown = md:
    let
      html = runExecline.local "rendered-markdown" {
        stdin = md;
      } [
        "importas" "-i" "out" "out"
        "redirfd" "-w" stdout "$out"
        bins.lowdown "-T" "html"
      ];
    in builtins.readFile html;

  indexTemplate = { title, description, pages ? [] }: ''
    <!doctype html>
    <html>
      <head>
        <meta charset="utf-8">
        <title>${title}</title>
        <link rel="stylesheet" type="text/css" href="style.css"/>
      </head>
      <body>
        <div class="index-text">
          <h1>${title}</h1>
          ${markdown description}
          <h2>man pages</h2>
          <ul>
            ${lib.concatMapStrings ({ name, section, ... }: ''
              <li><a href="${name}.${toString section}.html">${name}(${toString section})</a></li>
            '') pages}
          </ul>
        </div>
      </body>
    </html>
  '';

  defaultStyle = import ./defaultStyle.nix { };

  htmlman =
    { title
    # title of the index page
    , description ? ""
    # description which is displayed after
    # the main heading on the index page
    , pages ? []
    # man pages of the following structure:
    # {
    #   name : string;
    #   section : int;
    #   path : either path string;
    # }
    # path is optional, if it is not given,
    # the man page source must be located at
    # "${manDir}/${name}.${toString section}"
    , manDir ? null
    # directory in which man page sources are located
    , style ? defaultStyle
    # CSS to use as a string
    , normalizeCss ? true
    # whether to include normalize.css before the custom CSS
    }:

    let
      index = indexTemplate {
        inherit title description pages;
      };
      resolvePath = { path ? null, name, section }:
        if path != null
        then path
        else "${manDir}/${name}.${toString section}";
    in
      runExecline.local "htmlman-${title}" {
        derivationArgs = {
          inherit index style;
          passAsFile = [ "index" "style" ];
        };
      } ([
        "importas" "-i" "out" "out"
        "if" [ bins.mkdir "-p" "$out" ]
        "importas" "-i" "index" "indexPath"
        "if" [ bins.mv "$index" "\${out}/index.html" ]
        "importas" "-i" "style" "stylePath"
        "if" [
          "redirfd" "-w" stdout "\${out}/style.css"
          "if" ([
            bins.cat
          ] ++ lib.optional normalizeCss normalizeDrv
            ++ [
            "$style"
          ])
        ]
      ] ++ lib.concatMap ({ name, section, ... }@p: [
        "redirfd" "-w" stdout "\${out}/${name}.${toString section}.html"
        "if" [
          bins.mandoc
          "-T" "html" "-mdoc"
          "-O" "style=style.css"
          (resolvePath p)
        ]
      ]) pages);
in
  htmlman
