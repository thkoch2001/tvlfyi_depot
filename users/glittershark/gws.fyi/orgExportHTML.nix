{ pkgs, ... }:

with pkgs;
with lib;

let

  emacsWithPackages = (pkgs.emacsPackagesGen pkgs.emacs27).emacsWithPackages;

  emacs = emacsWithPackages (p: with p; [
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
