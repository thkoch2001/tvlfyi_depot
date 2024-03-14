# Configure public keys for SSH hosts known to TVL.
{ ... }:

{
  programs.ssh.knownHosts = {
    whitby = {
      hostNames = [
        "whitby.tvl.fyi"
        "whitby.tvl.su"
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I";
    };

    sanduny = {
      hostNames = [ "sanduny.tvl.su" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOag0XhylaTVhmT6HB8EN2Fv5Ymrc4ZfypOXONUkykTX";
    };

    github = {
      hostNames = [ "github.com" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    };
  };
}
