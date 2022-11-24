{ depot, pkgs, ... }:

let

  goStoreProtos = depot.nix.sparseTree ../store [
    ../store/protos
  ];
in

pkgs.buildGoModule
{
  name = "nar-bridge";
  vendorHash = "sha256-oFB+sLvhnsuQaCNvRemmZNJVg5FkhpMYXM0bvwcB73w=";
  src = ./.;

  preBuild = ''
    cp ${./go.mod} go.mod
    sed -i 's#=> ../store/protos#=> ${../store/protos}#' go.mod
    
    cat go.mod
  '';
}
