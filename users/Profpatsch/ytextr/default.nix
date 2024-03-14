{
  depot,
  pkgs,
  lib,
  ...
}:

# ytextr is a wrapper arount yt-dlp (previously youtube-dl)
# that extracts a single video according to my preferred settings.
#
# It will be sandboxed to the current directory, since I don’t particularly
# trust the massive codebase of that tool (with hundreds of contributors).
#
# Since the rules for downloading videos is usually against the wishes
# of proprietary vendors, and a video is many megabytes anyway,
# it will be fetched from the most recent nixpkgs unstable channel before running.

let
  bins = depot.nix.getBins pkgs.nix [ "nix-build" ] // depot.nix.getBins pkgs.bubblewrap [ "bwrap" ];

  # Run a command, with the given packages in scope, and `packageNamesAtRuntime` being fetched at the start in the given nix `channel`.
  nix-run-with-channel =
    {
      # The channel to get `packageNamesAtRuntime` from
      channel,
      # executable to run with `packageNamesAtRuntime` in PATH
      # and the argv
      executable,
      # A list of nixpkgs package attribute names that should be put into PATH when running `command`.
      packageNamesAtRuntime,
    }:
    depot.nix.writeExecline "nix-run-with-channel-${channel}" { } [
      # TODO: prevent race condition by writing a temporary gc root
      "backtick"
      "-iE"
      "storepath"
      [
        bins.nix-build
        "-I"
        "nixpkgs=channel:${channel}"
        "--arg"
        "packageNamesAtRuntimeJsonPath"
        (pkgs.writeText "packageNamesAtRuntime.json" (builtins.toJSON packageNamesAtRuntime))
        ./create-symlink-farm.nix
      ]
      "importas"
      "-ui"
      "PATH"
      "PATH"
      "export"
      "PATH"
      "\${storepath}/bin:\${PATH}"
      executable
      "$@"
    ];
in
nix-run-with-channel {
  channel = "nixos-unstable";
  packageNamesAtRuntime = [ "yt-dlp" ];
  executable = depot.nix.writeExecline "ytextr" { } [
    "getcwd"
    "-E"
    "cwd"
    bins.bwrap
    "--ro-bind"
    "/nix/store"
    "/nix/store"
    "--ro-bind"
    "/etc"
    "/etc"
    "--bind"
    "$cwd"
    "$cwd"
    "yt-dlp"
    "--no-playlist"
    "--write-sub"
    "--all-subs"
    "--embed-subs"
    "--merge-output-format"
    "mkv"
    "-f"
    "bestvideo[height<=?1080]+bestaudio/best"
    "$@"
  ];
}
