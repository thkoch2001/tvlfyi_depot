# If enabled, use Hadrian's Nix cache.
{ config, lib, pkgs, ... }:

{
  options = {
    hadrian.use-nix-cache.enable = lib.mkEnableOption "Hadrian's binary cache";
  };

  config = lib.mkIf config.hadrian.use-nix-cache.enable {
    nix.settings.trusted-public-keys = [
      "cache.hadrian.internal:XWdYSn5ZASj6IqZd4nnDBXJmahQEolBrtq9DvSe0UT0="
    ];
    nix.settings.substituters = [
      "http://cache.hadrian.internal"
    ];
  };
}
