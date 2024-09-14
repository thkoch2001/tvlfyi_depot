# radicle-explorer is the web UI for Radicle.
#
# They have an upstream Nix derivation, but it only works with experimental
# features Nix and is quite messy, so this is a copy of the relevant parts.
{ pkgs, ... }:

let
  twemoji-assets = pkgs.fetchFromGitHub {
    owner = "twitter";
    repo = "twemoji";
    rev = "v14.0.2";
    hash = "sha256-YoOnZ5uVukzi/6bLi22Y8U5TpplPzB7ji42l+/ys5xI=";
  };

  httpdSrc = pkgs.radicle-httpd.src;
in
pkgs.buildNpmPackage rec {
  pname = "radicle-explorer";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;

  # source should be synced with the httpd, which is already in nixpkgs
  src = pkgs.fetchgit {
    inherit (httpdSrc) url rev;
    hash = "sha256:09m13238h6j7g02r6332ihgyyzbjx90pgz14rz29pgv7936h6il8";
  };

  # This might change during nixpkgs bumps and will need updating. Need to fix
  # upstream so that there is a normal, callable derivation.
  npmDepsHash = "sha256:0kw6rvqm0s21j1rss35idvgcrzzczfy6qi3323y385djw4ygk5xs";

  postPatch = ''
    patchShebangs --build ./scripts
    mkdir -p "public/twemoji"
    cp -t public/twemoji -r -- ${twemoji-assets}/assets/svg/*
    : >scripts/install-twemoji-assets
  '';
  dontConfigure = true;
  doCheck = false;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r -t "$out" build/*
    runHook postInstall
  '';

}
