{ depot, ... }:

let
  inherit (depot.third_party.nixpkgs) lib;

  inherit (depot.users.sterni.nix) url fun string;

  inherit (depot.web.bubblegum) pathInfo scriptName respond absolutePath;

  # substituted using substituteAll in default.nix
  blogdir = "@blogdir@";
  # blogdir = toString ./posts; # for local testing

  parseDate = post:
    let matched = builtins.match "/?([0-9]+)-([0-9]+)-([0-9]+)-.+" post;
    in if matched == null then [
      0
      0
      0
    ] else
      builtins.map builtins.fromJSON matched;

  parseTitle = post:
    let matched = builtins.match "/?[0-9]+-[0-9]+-[0-9]+-(.+).html" post;
    in if matched == null then "no title" else builtins.head matched;

  dateAtLeast = a: b:
    builtins.all fun.id (lib.zipListsWith (partA: partB: partA >= partB) a b);

  byPostDate = a: b: dateAtLeast (parseDate a) (parseDate b);

  posts = builtins.sort byPostDate (builtins.attrNames
    (lib.filterAttrs (_: v: v == "regular") (builtins.readDir blogdir)));

  generic = { title, inner, ... }: ''
    <!doctype html>
    <html>
      <head>
        <meta charset="utf-8">
        <title>${title}</title>
        <style>a:link, a:visited { color: blue; }</style>
      </head>
      <body>
      ${inner}
      </body>
    </html>
  '';

  index = posts:
    ''
      <main>
        <h1>blog posts</h1>
        <ul>
    '' + lib.concatMapStrings (post: ''
      <li>
        <a href="${absolutePath (url.encode { } post)}">${parseTitle post}</a>
      </li>
    '') posts + ''
        </ul>
      </main>
    '';

  formatDate = let
    # Assume we never deal with years < 1000
    formatDigit = d:
      string.fit {
        char = "0";
        width = 2;
      } (toString d);
  in lib.concatMapStringsSep "-" formatDigit;

  post = title: post: ''
    <main>
      <h1>${title}</h1>
      <div id="content">
        ${builtins.readFile (blogdir + "/" + post)}
      </div>
    </main>
    <footer>
      <p>Posted on ${formatDate (parseDate post)}</p>
      <nav><a href="${scriptName}">index</a></nav>
    </footer>
  '';

  validatePathInfo = pathInfo:
    let chars = string.toChars pathInfo;
    in builtins.length chars > 1 && !(builtins.elem "/" (builtins.tail chars));

  response = if pathInfo == "/" then {
    title = "blog";
    status = 200;
    inner = index posts;
  } else if !(validatePathInfo pathInfo) then {
    title = "Bad Request";
    status = 400;
    inner = "No slashes in post names 😡";
  }
  # CGI should already url.decode for us
  else if builtins.pathExists (blogdir + "/" + pathInfo) then rec {
    title = parseTitle pathInfo;
    status = 200;
    inner = post title pathInfo;
  } else {
    title = "Not Found";
    status = 404;
    inner = "<h1>404 — not found</h1>";
  };
in respond response.status { "Content-type" = "text/html"; } (generic response)
