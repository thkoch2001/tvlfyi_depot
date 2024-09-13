# Display lyrics for the given search string;
# search string can contain a substring of band name, album name, song title
#
# Use the database dump from https://lrclib.net/db-dumps and place it in ~/.cache/lyric/lrclib-db-dump.sqlite3

{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.sqlite-utils [ "sqlite-utils" ]
    // depot.nix.getBins pkgs.jq [ "jq" ];

in
depot.nix.writeExecline "lyric" { readNArgs = 1; } [
  "backtick"
  "-E"
  "cache"
  [ depot.users.Profpatsch.xdg-cache-home ]
  "pipeline"
  [
    bins.sqlite-utils
    "query"
    "\${cache}/lyric/lrclib-db-dump.sqlite3"
    ''
      select
          synced_lyrics,
          source,
          t.name,
          t.artist_name
      from
          tracks_fts(:searchstring) tf
          join tracks t on t.rowid = tf.rowid
          join lyrics l on t.rowid = l.track_id
      order by
          t.id
      limit
          1
    ''
    "--param"
    "searchstring"
    "$1"
  ]
  bins.jq
  "-r"
  ".[0].synced_lyrics"
]
