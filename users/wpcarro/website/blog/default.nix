{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter readFile;
  inherit (depot.web.blog) post includePost renderPost;
  inherit (depot.users.wpcarro.website) domain renderTemplate withBrand;
  inherit (lib.lists) sort;

  config = {
    name = "bill and his blog";
    baseUrl = "https://${domain}/blog";
    staticUrl = "https://static.tvl.fyi/latest";
    footer = "";
  };

  posts = sort (x: y: x.date > y.date)
    (filter includePost (list post (import ./posts.nix)));

  rendered = pkgs.runCommandNoCC "blog-posts" { } ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${renderPost config post} $out/${post.key}.html"
    ) posts)}
  '';

  formatDate = date: readFile (pkgs.runCommandNoCC "date" { } ''
    date --date='@${toString date}' '+%B %e, %Y' > $out
  '');

  postsHtml = renderTemplate ./fragments/posts.html {
    postsHtml = lib.concatStringsSep "\n" (map toPostHtml posts);
  };

  toPostHtml = post: readFile (renderTemplate ./fragments/post.html {
    postUrl = "${config.baseUrl}/posts/${post.key}.html";
    postTitle = post.title;
    postDate = formatDate post.date;
  });
in
pkgs.runCommandNoCC "blog" { } ''
  mkdir -p $out
  cp ${withBrand (readFile postsHtml)} $out/index.html
  cp -r ${rendered} $out/posts
''
