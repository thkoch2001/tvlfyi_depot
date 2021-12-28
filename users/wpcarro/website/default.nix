{ pkgs, depot, ... }:

let
  inherit (builtins) readFile;
  inherit (depot.users) wpcarro;

  render = contentHtml: pkgs.substituteAll {
    inherit contentHtml;
    src = ./fragments/template.html;
  };
in {
  inherit render;

  root = pkgs.runCommandNoCC "wpcarro.dev" {} ''
    mkdir -p $out

    # /
    cp ${render (readFile ./fragments/homepage.html)} $out/index.html

    # /habits
    mkdir -p $out/habits
    cp -r ${wpcarro.website.habit-screens} $out/habits/index.html

    # /blog
    cp -r ${wpcarro.website.blog.root} $out/blog
  '';
}
