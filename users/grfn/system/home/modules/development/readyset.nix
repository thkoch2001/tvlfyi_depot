{ config, lib, pkgs, ... }:

{
  imports = [
    ./rust.nix
  ];

  home.packages = with pkgs; [
    # This goes in $PATH so I can run it from rofi and parent to my WM
    (writeShellScriptBin "dotclip" "xclip -out -selection clipboard | dot -Tpng | feh -")
  ];

  programs.zsh.shellAliases = {
    "tf" = "terraform";
  };
}
