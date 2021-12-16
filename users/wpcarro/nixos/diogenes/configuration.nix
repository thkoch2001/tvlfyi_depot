{ depot, pkgs, ... }:

let
  inherit (depot.users.wpcarro) keys;
in {
  imports = [
    "${depot.path}/ops/modules/tvl.nix"
    "${pkgs.path}/nixos/modules/virtualisation/google-compute-image.nix"
  ];

  # Use the TVL binary cache
  tvl.cache.enable = true;

  # Use 100G volume for /nix
  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/5a65f6c2-7d55-414d-b693-1928480c9b96";
    fsType = "ext4";
  };

  users = {
    mutableUsers = true;
    users = {
      wpcarro = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        openssh.authorizedKeys.keys = keys.all;
        shell = pkgs.fish;
      };
    };
  };


  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    fd
    fzf
    mkpasswd
    ripgrep
    tldr
    tree
    vim
  ];

  services = {
    depot.automatic-gc = {
      enable = true;
      interval = "1 hour";
      diskThreshold = 16; # GiB
      maxFreed = 10; # GiB
      preserveGenerations = "14d";
    };
  };

  system.stateVersion = "21.11";
}
