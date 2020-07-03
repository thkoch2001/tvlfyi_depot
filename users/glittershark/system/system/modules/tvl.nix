{ config, lib, pkgs, ... }:

{
  nix = {
    binaryCaches = ["ssh://nix-ssh@whitby.tvl.fyi"];
    trustedBinaryCaches = ["ssh://nix-ssh@whitby.tvl.fyi"];
    binaryCachePublicKeys = ["cache.tvl.fyi:fd+9d1ceCPvDX/xVhcfv8nAa6njEhAGAEe+oGJDEeoc="];
  };

  programs.ssh.knownHosts.whitby = {
    hostNames = [ "whitby" "whitby.tvl.fyi" "49.12.129.211"];
    publicKeyFile = pkgs.writeText "whitby.pub" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I
    '';
  };
}
