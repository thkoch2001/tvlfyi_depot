{ config, lib, pkgs, ... }:

{
  options = {
    tvl.cache.enable = lib.mkEnableOption "the TVL binary cache";
  };

  config = lib.mkIf config.tvl.cache.enable {
    nix.settings = {
      trusted-public-keys = [
        "cache.tvl.su:kjc6KOMupXc1vHVufJUoDUYeLzbwSr9abcAKdn/U1Jk="
      ];

      substituters = [
        "https://cache.tvl.su"
      ];
    };
  };
}
