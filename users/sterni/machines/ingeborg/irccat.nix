{
  depot,
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [ (depot.path.origSrc + "/ops/modules/irccat.nix") ];

  config = {
    services.depot.irccat = {
      enable = true;
      secretsFile = builtins.toFile "empty.json" "{}"; # TODO(sterni): register
      config = {
        tcp.listen = ":4722"; # ircc
        irc = {
          server = "irc.hackint.org:6697";
          tls = true;
          nick = config.networking.hostName;
          realname = "irccat";
        };
      };
    };
  };
}
