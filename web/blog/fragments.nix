# This file defines various fragments of the blog, such as the header
# and footer, as functions that receive arguments to be templated into
# them.
#
# An entire post is rendered by `renderPost`, which assembles the
# fragments together in a runCommand execution.
{ depot, lib, pkgs, ... }:

let
  inherit (builtins) filter map hasAttr replaceStrings;
  inherit (pkgs) runCommandNoCC writeText;
  inherit (depot.nix) renderMarkdown;

  staticUrl = "https://static.tvl.fyi/${depot.web.static.drvHash}";

  # Generate a post list for all listed, non-draft posts.
  isDraft = post: (hasAttr "draft" post) && post.draft;
  isUnlisted = post: (hasAttr "listed" post) && !post.listed;

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  header = name: title: ''
    <!DOCTYPE html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <meta name="description" content="${escape name}">
      <link rel="stylesheet" type="text/css" href="${staticUrl}/tvl.css" media="all">
      <link rel="icon" type="image/webp" href="/static/favicon.webp">
      <link rel="alternate" type="application/atom+xml" title="Atom Feed" href="https://tvl.fyi/feed.atom">
      <title>${escape name}: ${escape title}</title>
    </head>
    <body class="light">
      <header>
        <h1><a class="blog-title" href="/">${escape name}</a> </h1>
        <hr>
      </header>
  '';

  fullFooter = content: ''
      <hr>
      <footer>
        ${content}
      </footer>
    </body>
  '';

  draftWarning = writeText "draft.html" ''
    <p class="cheddar-callout cheddar-warning">
      <b>Note:</b> This post is a <b>draft</b>! Please do not share
      the link to it without asking first.
    </p>
    <hr>
  '';

  unlistedWarning = writeText "unlisted.html" ''
    <p class="cheddar-callout cheddar-warning">
      <b>Note:</b> This post is <b>unlisted</b>! Please do not share
      the link to it without asking first.
    </p>
    <hr>
  '';

  renderPost = { name, footer, ... }: post: runCommandNoCC "${post.key}.html" { } ''
    cat ${writeText "header.html" (header name post.title)} > $out

    # Write the post title & date
    echo '<article><h2 class="inline">${escape post.title}</h2>' >> $out
    echo '<aside class="date">' >> $out
    date --date="@${toString post.date}" '+%Y-%m-%d' >> $out
    ${
      if post ? updated
      then ''date --date="@${toString post.updated}" '+ (updated %Y-%m-%d)' >> $out''
      else ""
    }
    echo '</aside>' >> $out

    ${
      # Add a warning to draft/unlisted posts to make it clear that
      # people should not share the post.

      if (isDraft post) then "cat ${draftWarning} >> $out"
      else if (isUnlisted post) then "cat ${unlistedWarning} >> $out"
      else "# Your ads could be here?"
    }

    # Write the actual post through cheddar's about-filter mechanism
    cat ${renderMarkdown post.content} >> $out
    echo '</article>' >> $out

    cat ${writeText "footer.html" (fullFooter footer)} >> $out
  '';
in
{
  inherit isDraft isUnlisted renderPost;
}
