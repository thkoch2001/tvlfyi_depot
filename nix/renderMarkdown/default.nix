# Render a Markdown file to HTML.
{ depot, pkgs, ... }:

with depot.nix.yants;

defun [ path drv ] (file: pkgs.runCommand "${file}.rendered.html" { } ''
  cat ${file} | ${depot.tools.cheddar}/bin/cheddar --about-filter ${file} > $out
'')
