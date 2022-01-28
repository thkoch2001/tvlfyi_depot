{ depot
, pkgs
, ...
}:
pkgs.runCommandNoCC
  "monsterpoker.app"
  { }
  ''
  mkdir -p $out
  cp ${ ./index.html } $out/index.html
  ''
