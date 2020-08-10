# Render a Markdown file to HTML.
{ depot, ... }:

with depot.nix.yants;

defun [ path drv ] (file: depot.third_party.runCommandNoCC "${file}.rendered.html" {} ''
  cat ${file} | ${depot.tools.cheddar}/bin/cheddar --about-filter ${file} > $out
'')

