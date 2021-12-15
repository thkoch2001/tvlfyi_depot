{ config, lib, pkgs, ... }:

{
  options = {
    tvl.cache.enable = lib.mkEnable "the TVL binary cache";
  };

  config = lib.mkIf config.tvl.cache.enable {
    nix = {
      binaryCachePublicKeys = [
        "cache.tvl.su:kjc6KOMupXc1vHVufJUoDUYeLzbwSr9abcAKdn/U1Jk="
      ];

      binaryCaches = [
        "https://cache.tvl.su"
      ];
    };
  };
}
