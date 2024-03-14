# Render a Markdown file to HTML.
{ depot, pkgs, ... }:

with depot.nix.yants;

let
  args = struct "args" {
    path = path;
    tagfilter = option bool;
  };
in
defun
  [
    (either path args)
    drv
  ]
  (
    arg:
    pkgs.runCommand "${arg.path or arg}.rendered.html" { } (
      let
        tagfilter = if (arg.tagfilter or true) then "" else "--no-tagfilter";
      in
      ''
        cat ${arg.path or arg} | ${depot.tools.cheddar}/bin/cheddar --about-filter ${tagfilter} ${arg.path or arg} > $out
      ''
    )
  )
