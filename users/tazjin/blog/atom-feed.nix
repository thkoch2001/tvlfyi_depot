# This file defines functions for generating an Atom feed.

{ depot, lib, ... }:

with depot.nix.yants;

let
  inherit (builtins) map readFile replaceStrings;
  inherit (lib) concatStrings concatStringsSep removeSuffix;
  inherit (depot.third_party) runCommandNoCC;

  # 'link' describes a related link to a feed, or feed element.
  #
  # https://validator.w3.org/feed/docs/atom.html#link
  link = struct "link" {
    rel = string;
    href = string;
  };

  # 'entry' describes a feed entry, for example a single post on a
  # blog. Some optional fields have been omitted.
  #
  # https://validator.w3.org/feed/docs/atom.html#requiredEntryElements
  entry = struct {
    # Identifies the entry using a universally unique and permanent URI.
    id = string;

    # Contains a human readable title for the entry. This value should
    # not be blank.
    title = string;

    # Content of the entry. This element is technically optional, but
    # only if an alternate link is provided. In practice it should
    # always be present in the feeds generated by this code.
    content = string;

    # Indicates the last time the entry was modified in a significant
    # way (in seconds since epoch).
    updated = int;

    # Names authors of the entry. Recommended element.
    authors = option (list string);

    # Related web pages, such as the web location of a blog post.
    links = option (list link);

    # Conveys a short summary, abstract, or excerpt of the entry.
    summary = option string;

    # Contains the time of the initial creation or first availability
    # of the entry.
    published = option int;

    # Conveys information about rights, e.g. copyrights, held in and
    # over the entry.
    rights = option string;
  };

  # 'feed' describes the metadata of the Atom feed itself.
  #
  # Some optional fields have been omitted.
  #
  # https://validator.w3.org/feed/docs/atom.html#requiredFeedElements
  feed = struct "feed" {
    # Identifies the feed using a universally unique and permanent URI.
    id = string;

    # Contains a human readable title for the feed.
    title = string;

    # Indicates the last time the feed was modified in a significant
    # way (in seconds since epoch). Recommended element.
    updated = int;

    # Entries contained within the feed.
    entries = list entry;

    # Names authors of the feed. Recommended element.
    authors = option (list string);

    # Related web locations. Recommended element.
    links = option (list link);

    # Conveys information about rights, e.g. copyrights, held in and
    # over the feed.
    rights = option string;

    # Contains a human-readable description or subtitle for the feed.
    subtitle = option string;
  };

  # Feed generation functions:

  renderEpoch = epoch: removeSuffix "\n" (readFile (runCommandNoCC "$timestamp-{epoch}" {} ''
    date --date='@${toString epoch}' --utc --iso-8601='seconds' > $out
  ''));

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  elem = name: content: ''<${name}>${escape content}</${name}>'';

  renderLink = defun [ link string ] (l: ''
    <link href="${escape l.href}" rel="${escape l.rel}" />
  '');

  # Technically the author element can also contain 'uri' and 'email'
  # fields, but they are not used for the purpose of this feed and are
  # omitted.
  renderAuthor = author: ''<author><name>${escape author}</name></author>'';

  renderEntry = defun [ entry string] (e: ''
    <entry>
      ${elem "title" e.title}
      ${elem "id" e.id}
      ${elem "updated" (renderEpoch e.updated)}
      ${elem "content" e.content}
      ${concatStrings (map renderAuthor (e.authors or []))}
      ${if e ? subtitle then elem "subtitle" e.subtitle else ""}
      ${if e ? rights then elem "rights" e.rights else ""}
      ${concatStrings (map renderLink (e.links or []))}
    </entry>
  '');

  renderFeed = defun [ feed string ] (f: ''
    <?xml version="1.0" encoding="utf-8"?>
    <feed xmlns="http://www.w3.org/2005/Atom">
      ${elem "id" f.id}
      ${elem "title" f.title}
      ${elem "updated" (renderEpoch f.updated)}
      ${concatStringsSep "\n" (map renderAuthor (f.authors or []))}
      ${if f ? subtitle then elem "subtitle" f.subtitle else ""}
      ${if f ? rights then elem "rights" f.rights else ""}
      ${concatStrings (map renderLink (f.links or []))}
      ${concatStrings (map renderEntry f.entries)}
    </feed>
  '');
in {
  inherit entry feed renderFeed renderEpoch;
}
