# Creates the Atom feed for my homepage.
{ depot, lib, pkgs, entry, pageEntries, ... }:

with depot.nix.yants;

let
  inherit (builtins) map readFile sort foldl';
  inherit (lib) max singleton;
  inherit (pkgs) writeText;
  inherit (depot.users.tazjin) atom-feed blog renderMarkdown;

  postToEntry = defun [ blog.post atom-feed.entry ] (post: rec {
    id = "https://tazj.in/blog/${post.key}";
    title = post.title;
    content = readFile (renderMarkdown post.content);
    published = post.date;
    updated = post.updated or post.date;

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

  mostRecentlyUpdated = foldl' max 0 (map (e: e.updated) allEntries);

  feed = {
    id = "https://tazj.in/";
    title = "tazjin's interblag";
    subtitle = "my posts, projects and other interesting things";
    updated = mostRecentlyUpdated;
    rights = "© 2020 tazjin";
    authors = [ "tazjin" ];

    links = singleton {
      rel = "self";
      href = "https://tazjin/feed.atom";
    };

    entries = sort (a: b: a.published > b.published) allEntries;
  };
in writeText "feed.atom" (atom-feed.renderFeed feed)
