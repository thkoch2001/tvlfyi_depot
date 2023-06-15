# Assembles the website index and configures an nginx instance to
# serve it.
#
# The website is made up of a simple header&footer and content
# elements for things such as blog posts and projects.
#
# Content for the blog is in //users/tazjin/blog instead of here.
{ depot, lib, pkgs, ... }@args:

with depot;
with nix.yants;

let
  inherit (builtins) readFile replaceStrings sort;
  inherit (pkgs) writeFile runCommand;

  # The different types of entries on the homepage.
  entryClass = enum "entryClass" [
    "blog"
    "project"
    "note"
    "misc"
  ];

  # The definition of a single entry.
  entry = struct "entry" {
    class = entryClass;
    title = option string;
    url = option string;
    date = int; # epoch
    description = option string;
  };

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  postToEntry = defun [ web.blog.post entry ] (post: {
    class = "blog";
    title = post.title;
    url = "/blog/${post.key}";
    date = post.date;
    description = post.description or "Blog post from ${formatDate post.date}";
  });

  formatDate = defun [ int string ] (date: readFile (runCommand "date" { } ''
    date --date='@${toString date}' '+%Y-%m-%d' | tr -d '\n' > $out
  ''));

  entryUrl = defun [ entry string ] (entry:
    if entry.class == "note"
    then "#${toString entry.date}"
    else entry.url
  );

  hasDescription = defun [ entry bool ] (entry:
    ((entry ? description) && (entry.description != null))
  );

  entryTitle = defun [ entry string ] (entry:
    let
      optionalColon = lib.optionalString (hasDescription entry) ":";
      titleText =
        if (!(entry ? title) && (entry.class == "note"))
        then "[${formatDate entry.date}]"
        else lib.optionalString (entry ? title) ((escape entry.title) + optionalColon);
    in
    lib.optionalString (titleText != "")
      ''<span class="entry-title ${entry.class}">${titleText}</span>''
  );

  entryToDiv = defun [ entry string ] (entry: ''
    <a href="${entryUrl entry}" id="${toString entry.date}" class="entry">
      ${entryTitle entry}
      ${
        lib.optionalString (hasDescription entry)
        "<span class=\"entry-description\">${escape entry.description}</span>"
      }
    </a>
  '');

  index = entries: pkgs.writeText "index.html" (lib.concatStrings (
    [ (builtins.readFile ./header.html) ]
    ++ (map entryToDiv (sort (a: b: a.date > b.date) entries))
    ++ [ (builtins.readFile ./footer.html) ]
  ));

  pageEntries = import ./entries.nix;
  homepage = index ((map postToEntry users.tazjin.blog.posts) ++ pageEntries);
  atomFeed = import ./feed.nix (args // { inherit entry pageEntries; });
in
runCommand "website" { } ''
  mkdir $out
  cp ${homepage} $out/index.html
  cp ${atomFeed} $out/feed.atom
  mkdir $out/static
  cp -r ${depot.web.static}/* $out/static
  cp -rf ${./static}/* $out/static
''
