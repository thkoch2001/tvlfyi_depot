{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter;

  config = {
    name = "tazjin's blog";
    baseUrl = "https://tazj.in/blog";

    footer = ''
      <p class="footer">
        <a class="uncoloured-link" href="https://tazj.in">homepage</a>
        |
        <a class="uncoloured-link" href="https://cs.tvl.fyi/">code</a>
      </p>
      <p class="lod">ಠ_ಠ</p>
    '';
  };

  inherit (depot.web.blog) post includePost renderPost;

  posts = filter includePost (list post (import ./posts.nix));

  rendered = pkgs.runCommandNoCC "tazjins-blog" { } ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${renderPost config post} $out/${post.key}.html"
    ) posts)}
  '';

in
{
  inherit posts rendered config;

  # Generate embeddable nginx configuration for redirects from old post URLs
  oldRedirects = lib.concatStringsSep "\n" (map
    (post: ''
      location ~* ^(/en)?/${post.oldKey} {
        return 301 https://tazj.in/blog/${post.key};
      }
    '')
    (filter (hasAttr "oldKey") posts));
}
