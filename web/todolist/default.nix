# Generates a simple web view of open TODOs in the depot.
#
# Only TODOs that match the form 'TODO($username)' are considered, and
# only for users that are known to us.
{ depot, lib, ... }:

with depot.nix.yants;

let
  inherit (depot.third_party)
    jq
    ripgrep
    runCommandNoCC
    writeText
    ;

  inherit (builtins)
    elem
    filter
    fromJSON
    head
    readFile
    map
    ;

  inherit (lib) concatStringsSep;

  knownUsers = map (u: u.username) depot.ops.users;

  todo = struct {
    file = string;
    line = int;
    todo = string;
    user = string;
  };

  allTodos = fromJSON (readFile (runCommandNoCC "depot-todos.json" {} ''
    ${ripgrep}/bin/rg --json 'TODO\(\w+\):.*$' ${depot.depotPath} | \
      ${jq}/bin/jq -s -f ${./extract-todos.jq} > $out
  ''));

  knownUserTodos = filter (todos: elem (head todos).user knownUsers) allTodos;

  fileLink = defun [ todo string ] (t:
    ''<a style="color: inherit;"
         href="https://cs.tvl.fyi/depot/-/blob/${t.file}#L${toString t.line}">
      //${t.file}:${toString t.line}</a>'');

  todoElement = defun [ todo string ] (t: ''
    <p>${fileLink t}:</p>
    <blockquote>${t.todo}</blockquote>

  '');

  userParagraph = todos:
  let user = (head todos).user;
  in ''
    <p>
      <h3>
        <a style="color:inherit; text-decoration: none;"
           name="${user}"
           href="#${user}">${user}</a>
      </h3>
      ${concatStringsSep "\n" (map todoElement todos)}
    </p>
    <hr>
  '';

  todoPage = writeText "index.html" ''
    <!DOCTYPE html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <meta name="description" content="TVL's todo-list">
      <link rel="stylesheet" type="text/css" href="static/tazjin.css" media="all">
      <link rel="icon" type="image/webp" href="static/favicon.webp">
      <title>TVL's todo-list</title>
      <style>
        svg {
          max-width: inherit;
          height: auto;
        }
      </style>
    </head>
    <body class="dark">
      <header>
        <h1><a class="blog-title" href="/">The Virus Lounge's todo-list</a> </h1>
        <hr>
      </header>
      <main>
      ${concatStringsSep "\n" (map userParagraph knownUserTodos)}
      </main>
      <footer>
        <p class="footer">
          <a class="uncoloured-link" href="https://tvl.fyi">homepage</a>
          |
          <a class="uncoloured-link" href="https://cs.tvl.fyi/depot/-/blob/README.md">code</a>
          |
          <a class="uncoloured-link" href="https://cl.tvl.fyi">reviews</a>
        </p>
        <p class="lod">ಠ_ಠ</p>
      </footer>
    </body>
  '';

in runCommandNoCC "tvl-todos" {} ''
  mkdir $out
  cp ${todoPage} $out/index.html
  ln -s ${depot.web.tvl}/static $out/static
''
