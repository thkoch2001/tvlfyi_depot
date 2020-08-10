# Creates the Atom feed for my homepage.
{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) map readFile;
  inherit (lib) singleton;
  inherit (pkgs) writeText;
  inherit (depot.users.tazjin) atom-feed blog renderMarkdown;

  postToEntry = defun [ blog.post atom-feed.entry ] (post: rec {
    id = "https://tazj.in/blog/${post.key}";
    title = post.title;
    content = readFile (renderMarkdown post.content);
    published = post.date;
    updated = post.date; # TODO(tazjin): this should be distinct from published

    links = singleton {
      rel = "alternate";
      href = id;
    };
  });

  feed = {
    id = "https://tazj.in/";
    title = "tazjin's interblag";
    # TODO(tazjin): Take the most recently updated entry time instead.
    updated = builtins.currentTime;
    rights = "© 2020 tazjin";
    authors = [ "tazjin" ];

    links = singleton {
      rel = "self";
      href = "https://tazjin/feed.atom";
    };

    entries = map postToEntry blog.posts;
  };
in writeText "feed.atom" (atom-feed.renderFeed feed)
