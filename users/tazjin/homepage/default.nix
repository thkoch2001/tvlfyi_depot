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
  inherit (pkgs) writeFile runCommandNoCC;

  # The different types of entries on the homepage.
  entryClass = enum "entryClass" [ "blog" "project" "misc" ];

  # The definition of a single entry.
  entry = struct "entry" {
    class = entryClass;
    title = string;
    url = string;
    date = int; # epoch
    description = option string;
  };

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  postToEntry = defun [ web.blog.post entry ] (post: {
    class = "blog";
    title = post.title;
    url = "/blog/${post.key}";
    date = post.date;
  });

  formatDate = defun [ int string ] (date: readFile (runCommandNoCC "date" { } ''
    date --date='@${toString date}' '+%Y-%m-%d' > $out
  ''));

  formatEntryDate = defun [ entry string ] (entry: entryClass.match entry.class {
    blog = "Blog post from ${formatDate entry.date}";
    project = "Project from ${formatDate entry.date}";
    misc = "Posted on ${formatDate entry.date}";
  });

  entryToDiv = defun [ entry string ] (entry: ''
    <a href="${entry.url}" class="entry ${entry.class}">
      <div>
        <p class="entry-title">${escape entry.title}</p>
        ${
          lib.optionalString ((entry ? description) && (entry.description != null))
          "<p class=\"entry-description\">${escape entry.description}</p>"
        }
        <p class="entry-date">${formatEntryDate entry}</p>
      </div>
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
runCommandNoCC "website" { } ''
  mkdir $out
  cp ${homepage} $out/index.html
  cp ${atomFeed} $out/feed.atom
  mkdir $out/static
  cp -r ${depot.web.static}/* $out/static
  cp -rf ${./static}/* $out/static
''
