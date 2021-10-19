{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter;
  inherit (depot.web.blog) post includePost renderPost;

  posts = filter includePost (list post (import ./posts.nix));

  rendered = pkgs.runCommandNoCC "tazjins-blog" {} ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${fragments.renderPost post} $out/${post.key}.html"
    ) posts)}
  '';

in {
  inherit posts rendered;

  # Generate embeddable nginx configuration for redirects from old post URLs
  oldRedirects = lib.concatStringsSep "\n" (map (post: ''
    location ~* ^(/en)?/${post.oldKey} {
      return 301 https://tazj.in/blog/${post.key};
    }
  '') (filter (hasAttr "oldKey") posts));
}
