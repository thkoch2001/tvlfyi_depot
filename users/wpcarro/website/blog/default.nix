{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter;
  inherit (depot.web.blog) post includePost renderPost;

  config = {
    name = "wpcarro's blog";
    baseUrl = "https://blog.wpcarro.dev";
    footer = "";
  };

  posts = filter includePost (list post (import ./posts.nix));

  rendered = pkgs.runCommandNoCC "wpcarros-blog" {} ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${renderPost config post} $out/${post.key}.html"
    ) posts)}
  '';

in {
  inherit posts rendered config;
}
