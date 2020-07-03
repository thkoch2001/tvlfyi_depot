{ config, lib, pkgs, ... }:

{
  nix = {
    buildMachines = [{
      hostName = "whitby.tvl.fyi";
      sshUser = "grfn";
      sshKey = "/root/.ssh/id_rsa.whitby";
      system = "x86_64-linux";
      maxJobs = 64;
      supportedFeatures = ["big-parallel"];
    }];

    binaryCaches = ["ssh://grfn@whitby.tvl.fyi"];
  };

  programs.ssh.knownHosts.whitby = {
    hostNames = [ "whitby" "whitby.tvl.fyi" "49.12.129.211"];
    publicKeyFile = pkgs.writeText "whitby.pub" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I
    '';
  };
}
