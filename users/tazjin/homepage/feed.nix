# Creates the Atom feed for my homepage.
{ depot, lib, pkgs, entry, pageEntries, ... }:

with depot.nix.yants;

let
  inherit (builtins) map readFile sort;
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

  pageEntryToEntry = defun [ entry atom-feed.entry ] (e: {
    id = "tazjin:${e.class}:${toString e.date}";
    updated = e.date;
    published = e.date;
    title = e.title;
    summary = e.description;

    links = singleton {
      rel = "alternate";
      href = e.url;
    };
  });

  allEntries = (map postToEntry blog.posts) ++ (map pageEntryToEntry pageEntries);

  feed = {
    id = "https://tazj.in/";
    title = "tazjin's interblag";
    subtitle = "my posts, projects and other interesting things";
    # TODO(tazjin): Take the most recently updated entry time instead.
    updated = builtins.currentTime;
    rights = "© 2020 tazjin";
    authors = [ "tazjin" ];

    links = singleton {
      rel = "self";
      href = "https://tazjin/feed.atom";
    };

    entries = sort (a: b: a.published > b.published) allEntries;
  };
in writeText "feed.atom" (atom-feed.renderFeed feed)
