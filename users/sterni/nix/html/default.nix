{ lib, ... }:

let
  /* Escape everything we have to escape in an HTML document if either
     in a normal context or an attribute string (`<>&"'`).

     A shorthand for this function called `esc` is also provided.

     Type: string -> string

     Example:

     escapeMinimal "<hello>"
     => "&lt;hello&gt;"
  */
  escapeMinimal = builtins.replaceStrings
    [ "<"    ">"    "&"     "\""     "'"      ]
    [ "&lt;" "&gt;" "&amp;" "&quot;" "&#039;" ];

  /* Return a string with a correctly rendered tag of the given name,
     with the given attributes which are automatically escaped.

     If the content argument is `null`, the tag will have no children nor a
     closing element. If the content argument is a string it is used as the
     content as is (unescaped). If the content argument is a list, its
     elements are concatenated.

     `renderTag` is only an internal function which is reexposed as `__findFile`
     to allow for much neater syntax than calling `renderTag` everywhere:

     ```nix
     { depot, ... }:
     let
       inherit (depot.users.sterni.nix.html) __findFile esc;
     in

     <html> {} [
       (<head> {} (<title> {} (esc "hello world")))
       (<body> {} [
         (<h1> {} (esc "hello world"))
         (<p> {} (esc "foo bar"))
       ])
     ]

     ```

     As you can see, the need to call a function disappears, instead the
     `NIX_PATH` lookup operation via `<foo>` is overloaded, so it becomes
     `renderTag "foo"` automatically.

     Since the content argument may contain the result of other `renderTag`
     calls, we can't escape it automatically. Instead this must be done manually
     using `esc`.

     Type: string -> attrs<string> -> (list<string> | string | null) -> string

     Example:

     <link> {
       rel = "stylesheet";
       href = "/css/main.css";
       type = "text/css";
     } null

     renderTag "link" {
       rel = "stylesheet";
       href = "/css/main.css";
       type = "text/css";
     } null

     => "<link href=\"/css/main.css\" rel=\"stylesheet\" type=\"text/css\"/>"

     <p> {} [
       "foo "
       (<strong> {} "bar")
     ]

     renderTag "p" {} "foo <strong>bar</strong>"
     => "<p>foo <strong>bar</strong></p>"
  */
  renderTag = tag: attrs: content:
    let
      attrs' = lib.concatMapStrings (n:
        " ${escapeMinimal n}=\"${escapeMinimal (toString attrs.${n})}\""
      ) (builtins.attrNames attrs);
      content' =
        if builtins.isList content
        then lib.concatStrings content
        else content;
    in
      if content == null
      then "<${tag}${attrs'}/>"
      else "<${tag}${attrs'}>${content'}</${tag}>";

  /* Prepend "<!DOCTYPE html>" to a string.

     Type: string -> string

     Example:

     withDoctype (<body> {} (esc "hello"))
     => "<!DOCTYPE html><body>hello</body>"
  */
  withDoctype = doc: "<!DOCTYPE html>" + doc;

in {
  inherit escapeMinimal renderTag withDoctype;

  __findFile = _: renderTag;
  esc = escapeMinimal;
}
