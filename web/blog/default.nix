# This creates the static files that make up my blog from the Markdown
# files in this repository.
#
# All blog posts are rendered from Markdown by cheddar.
{ depot, lib, pkgs, ... }@args:

with depot.nix.yants;

let
  inherit (builtins) readFile;
  inherit (depot.nix) renderMarkdown;
  inherit (depot.web) atom-feed;
  inherit (lib) singleton;

  # Type definition for a single blog post.
  post = struct "blog-post" {
    key = string;
    title = string;
    date = int;

    # Optional time at which this post was last updated.
    updated = option int;

    # Path to the Markdown file containing the post content.
    content = path;

    # Should this post be included in the index? (defaults to true)
    listed = option bool;

    # Is this a draft? (adds a banner indicating that the link should
    # not be shared)
    draft = option bool;

    # Previously each post title had a numeric ID. For these numeric
    # IDs, redirects are generated so that old URLs stay compatible.
    oldKey = option string;
  };

  # Rendering fragments for the HTML version of the blog.
  fragments = import ./fragments.nix args;

  # Functions for generating feeds for these blogs using //web/atom-feed.
  toFeedEntry = { baseUrl, ... }: defun [ post atom-feed.entry ] (post: rec {
    id = "${baseUrl}/${post.key}";
    title = post.title;
    content = readFile (renderMarkdown post.content);
    published = post.date;
    updated = post.updated or post.date;

    links = singleton {
      rel = "alternate";
      href = id;
    };
  });
in
{
  inherit post toFeedEntry;
  inherit (fragments) renderPost;

  # Helper function to determine whether a post should be included in
  # listings (on homepages, feeds, ...)
  includePost = post: !(fragments.isDraft post) && !(fragments.isUnlisted post);
}
