# This creates the static files that make up my blog from the Markdown
# files in this repository.
#
# All blog posts are rendered from Markdown by cheddar.
{ depot, lib, pkgs, ... }@args:

with depot.nix.yants;

let
  # Type definition for a single blog post.
  post = struct "blog-post" {
    key = string; #
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

  posts = list post (import ./posts.nix);
  fragments = import ./fragments.nix args;

  rendered = pkgs.runCommandNoCC "tazjins-blog" {} ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${fragments.renderPost post} $out/${post.key}.html"
    ) posts)}
  '';

  includePost = post: !(fragments.isDraft post) && !(fragments.isUnlisted post);
in {
  inherit post includePost;
  inherit (fragments) renderPost;
}
