{ pkgs, depot, ... }:

let
  inherit (depot.users.sterni.nix.html)
    __findFile
    withDoctype
    ;
in

{
  imports = [
    ./nginx.nix
  ];

  config = {
    services.nginx.virtualHosts."sterni.lv" = {
      enableACME = true;
      forceSSL = true;
      # TODO(sterni): tmp.sterni.lv
      root = pkgs.writeTextFile {
        name = "sterni.lv-http-root";
        destination = "/index.html";
        text = withDoctype (<html> {} [
          (<head> {} [
            (<meta> { charset = "utf-8"; } null)
            (<title> {} "no thoughts")
          ])
          (<body> {} "🦩")
        ]);
      };
    };
  };
}
