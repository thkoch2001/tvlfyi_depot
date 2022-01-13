{ pkgs, ... }:

pkgs.runCommandNoCC "demo-app" {} ''
  export HOME=$PWD
  mkdir -p $out/bin
  cp ${./main.go} main.go
  ${pkgs.go}/bin/go build main.go
  mv main $out/bin/demo-app
''
