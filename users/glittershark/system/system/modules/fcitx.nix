{ config, lib, pkgs, ... }:

{
  i18n.inputMethod = {
    enabled = "fcitx";
    engines = with pkgs.fcitx-engines; [
      cloudpinyin
    ];
  };
}
