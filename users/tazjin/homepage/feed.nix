# Creates the Atom feed for my homepage.
{ depot, lib, pkgs, entry, pageEntries, ... }:

with depot.nix.yants;

let
  inherit (builtins) map readFile;
  inherit (lib) max singleton;
  inherit (pkgs) writeText;
  inherit (depot.web) blog atom-feed;

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

  allEntries = (map blog.toFeedEntry depot.users.tazjin.blog.posts)
             ++ (map pageEntryToEntry pageEntries);

  feed = {
    id = "https://tazj.in/";
    title = "tazjin's interblag";
    subtitle = "my posts, projects and other interesting things";
    rights = "© 2020 tazjin";
    authors = [ "tazjin" ];

    links = singleton {
      rel = "self";
      href = "https://tazjin/feed.atom";
    };

    entries = allEntries;
  };
in writeText "feed.atom" (atom-feed.renderFeed feed)
