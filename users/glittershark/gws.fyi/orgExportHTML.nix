{ pkgs ? import <nixpkgs> {}, ... }:

with pkgs;
with lib;

let

  emacs-nixpkgs =
    (import <nixpkgs> {
      overlays = [(import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/54afb061bdd12c61bbfcc13bad98b7a3aab7d8d3.tar.gz";
        sha256 = "0hrbg65d5h0cb0nky7a46md7vlvhajq1hf0328l2f7ln9hznqz6j";
      }))];
    });

  emacs = (emacs-nixpkgs.emacsPackagesFor emacs-nixpkgs.emacsUnstable)
    .emacsWithPackages (p: with p; [
      org
    ]);

in

opts:

let
  src = if isAttrs opts then opts.src else opts;
  headline = if isAttrs opts then opts.headline else null;

  bn = builtins.baseNameOf src;
  filename = elemAt (splitString "." bn) 0;

  outName =
    if isNull headline
    then
      let bn = builtins.baseNameOf src;
          filename = elemAt (splitString "." bn) 0;
      in filename + ".html"
    else "${filename}-${replaceStrings [" "] ["-"] filename}.html";

  escapeDoubleQuotes = replaceStrings ["\""] ["\\\""];

  navToHeadline = optionalString (! isNull headline) ''
    (search-forward "${escapeDoubleQuotes headline}")
    (org-narrow-to-subtree)
  '';

in

runCommand outName {} ''
  cp ${src} file.org
  echo "${emacs}/bin/emacs --batch"
  ${emacs}/bin/emacs --batch \
    --load ${./config.el} \
    --visit file.org \
    --eval "(progn
      ${escapeDoubleQuotes navToHeadline}
      (org-html-export-to-html))" \
    --kill
  substitute file.html $out \
    --replace '<title>&lrm;</title>' ""
''
