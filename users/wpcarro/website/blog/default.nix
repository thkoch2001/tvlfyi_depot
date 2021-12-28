{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter readFile;
  inherit (depot.web.blog) post includePost renderPost;
  inherit (depot.users) wpcarro;

  config = {
    name = "wpcarro's blog";
    baseUrl = "https://wpcarro.dev/blog";
    footer = "";
  };

  posts = filter includePost (list post (import ./posts.nix));

  rendered = pkgs.runCommandNoCC "wpcarros-blog-posts" {} ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${renderPost config post} $out/${post.key}.html"
    ) posts)}
  '';

  formatDate = date: readFile (pkgs.runCommandNoCC "date" {} ''
    date --date='@${toString date}' '+%B %e, %Y' > $out
  '');

  postsHtml = readFile (pkgs.substituteAll {
    src = ./fragments/posts.html;
    postsHtml = lib.concatStringsSep "\n" (map toPostHtml posts);
  });

  toPostHtml = post: readFile (pkgs.substituteAll {
    src = ./fragments/post.html;
    postUrl = "${config.baseUrl}/${post.key}.html";
    postTitle = post.title;
    postDate = formatDate post.date;
  });
in {
  inherit posts rendered config;

  root = pkgs.runCommandNoCC "wpcarros-blog" {} ''
    mkdir -p $out
    cp ${wpcarro.website.render postsHtml} $out/index.html
  '';
}
