{ depot, lib, pkgs, ... }:

let
  inherit (depot.nix)
    getBins
    runExecline
    yants
    ;

  inherit (depot.tools)
    cheddar
    ;

  inherit (pkgs)
    mandoc
    coreutils
    fetchurl
    writers
    ;

  bins = getBins cheddar [ "cheddar" ]
    // getBins mandoc [ "mandoc" ]
    // getBins coreutils [ "cat" "mv" "mkdir" ]
  ;

  normalizeDrv = fetchurl {
    url = "https://necolas.github.io/normalize.css/8.0.1/normalize.css";
    sha256 = "04jmvybwh2ks4dlnfa70sb3a3z3ig4cv0ya9rizjvm140xq1h22q";
  };

  execlineStdoutInto = target: line: [
    "redirfd"
    "-w"
    "1"
    target
  ] ++ line;

  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  # I will not write a pure nix markdown renderer
  markdown = md:
    let
      html = runExecline.local "rendered-markdown"
        {
          stdin = md;
        }
        ([
          "importas"
          "-iu"
          "out"
          "out"
        ] ++ execlineStdoutInto "$out" [
          bins.cheddar
          "--about-filter"
          "description.md"
        ]);
    in
    builtins.readFile html;

  indexTemplate = { title, description, pages ? [ ] }: ''
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

  # This deploy script automatically copies the build result into
  # a TARGET directory and marks it as writeable optionally.
  # It is exposed as the deploy attribute of the result of
  # htmlman, so an htmlman expression can be used like this:
  # nix-build -A deploy htmlman.nix && ./result target_dir
  deployScript = title: drv: writers.writeDash "deploy-${title}" ''
    usage() {
      printf 'Usage: %s [-w] TARGET\n\n' "$0"
      printf 'Deploy htmlman documentation to TARGET directory.\n\n'
      printf '  -h    Display this help message\n'
      printf '  -w    Make TARGET directory writeable\n'
    }

    if test "$#" -lt 1; then
      usage
      exit 100
    fi

    writeable=false

    while test "$#" -gt 0; do
      case "$1" in
        -h)
          usage
          exit 0
          ;;
        -w)
          writeable=true
          ;;
        -*)
          usage
          exit 100
          ;;
        *)
          if test -z "$target"; then
            target="$1"
          else
            echo "Too many arguments"
            exit 100
          fi
          ;;
      esac

      shift
    done

    if test -z "$target"; then
      echo "Missing TARGET"
      usage
      exit 100
    fi

    set -ex

    mkdir -p "$target"
    cp -RTL --reflink=auto "${drv}" "$target"

    if $writeable; then
      chmod -R +w "$target"
    fi
  '';

  htmlman =
    { title
      # title of the index page
    , description ? ""
      # description which is displayed after
      # the main heading on the index page
    , pages ? [ ]
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
    , linkXr ? "all"
      # How to handle cross references in the html output:
      #
      # * none:     don't convert cross references into hyperlinks
      # * all:      link all cross references as if they were
      #             rendered into $out by htmlman
      # * inManDir: link to all man pages which have their source
      #             in `manDir` and use the format string defined
      #             in linkXrFallback for all other cross references.
    , linkXrFallback ? "https://manpages.debian.org/unstable/%N.%S.en.html"
      # fallback link to use if linkXr == "inManDir" and the man
      # page is not in ${manDir}. Placeholders %N (name of page)
      # and %S (section of page) can be used. See mandoc(1) for
      # more information.
    }:

    let
      linkXrEnum = yants.enum "linkXr" [ "all" "inManDir" "none" ];

      index = indexTemplate {
        inherit title description pages;
      };

      resolvePath = { path ? null, name, section }:
        if path != null
        then path
        else "${manDir}/${name}.${toString section}";

      mandocOpts = lib.concatStringsSep "," ([
        "style=style.css"
      ] ++ linkXrEnum.match linkXr {
        all = [ "man=./%N.%S.html" ];
        inManDir = [ "man=./%N.%S.html;${linkXrFallback}" ];
        none = [ ];
      });

      html =
        runExecline.local "htmlman-${title}"
          {
            derivationArgs = {
              inherit index style;
              passAsFile = [ "index" "style" ];
            };
          }
          ([
            "multisubstitute"
            [
              "importas"
              "-iu"
              "out"
              "out"
              "importas"
              "-iu"
              "index"
              "indexPath"
              "importas"
              "-iu"
              "style"
              "stylePath"
            ]
            "if"
            [ bins.mkdir "-p" "$out" ]
            "if"
            [ bins.mv "$index" "\${out}/index.html" ]
            "if"
            (execlineStdoutInto "\${out}/style.css" [
              "if"
              ([
                bins.cat
              ] ++ lib.optional normalizeCss normalizeDrv
              ++ [
                "$style"
              ])
            ])
            # let mandoc check for available man pages
            "execline-cd"
            "${manDir}"
          ] ++ lib.concatMap
            ({ name, section, ... }@p:
              execlineStdoutInto "\${out}/${name}.${toString section}.html" [
                "if"
                [
                  bins.mandoc
                  "-mdoc"
                  "-T"
                  "html"
                  "-O"
                  mandocOpts
                  (resolvePath p)
                ]
              ])
            pages);
    in
    html // {
      deploy = deployScript title html;
    };
in
htmlman
