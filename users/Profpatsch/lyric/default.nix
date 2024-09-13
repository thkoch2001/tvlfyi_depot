{ pkgs, depot, lib, ... }:

let
  bins = depot.nix.getBins pkgs.sqlite-utils [ "sqlite-utils" ]
    // depot.nix.getBins pkgs.jq [ "jq" ];

  mpv-script = pkgs.writeTextFile {
    name = "lyric.lua";
    text =
      lib.replaceStrings
        [ "@get_subtitles_command@" ]
        [ (toString lyric-to-temp-file) ]
        (builtins.readFile ./lyric-mpv-script.lua);
    derivationArgs.passthru.scriptName = "lyric.lua";
  };

  lyric-to-temp-file = depot.nix.writeExecline "lyric-to-temp-file" { readNArgs = 1; } [
    "backtick"
    "-E"
    "cache"
    [ depot.users.Profpatsch.xdg-cache-home ]
    "if"
    [ "mkdir" "-p" "\${cache}/lyric/as-files" ]
    "if"
    [
      "redirfd"
      "-w"
      "1"
      "\${cache}/lyric/as-files/\${1}.lrc"
      lyric
      "$1"
    ]
    "printf"
    "\${cache}/lyric/as-files/\${1}.lrc"
  ];

  # Display lyrics for the given search string;
  # search string can contain a substring of band name, album name, song title
  #
  # Use the database dump from https://lrclib.net/db-dumps and place it in ~/.cache/lyric/lrclib-db-dump.sqlite3
  lyric =

    (depot.nix.writeExecline "lyric" { readNArgs = 1; } [
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
              has_synced_lyrics,
              plain_lyrics
          from
              tracks_fts(:searchstring) tf
              join tracks t on t.rowid = tf.rowid
              join lyrics l on t.rowid = l.track_id
          order by
              has_synced_lyrics desc, t.id
          limit
              1
        ''
        "--param"
        "searchstring"
        "$1"
      ]
      bins.jq
      "-r"
      ''
        if .[0] == null
        then ""
        else
        .[0]
            | if .has_synced_lyrics == 1
            then .synced_lyrics
            else .plain_lyrics
            end
        end
      ''
    ]);


in
{
  inherit
    lyric
    mpv-script;
}
