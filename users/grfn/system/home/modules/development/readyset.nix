{ config, lib, pkgs, ... }:

{
  imports = [
    ./rust.nix
  ];

  home.packages = with pkgs; [
    # These go in $PATH so I can run it from rofi and parent to my WM
    (writeShellScriptBin "dotclip" "xclip -out -selection clipboard | dot -Tpng | feh -")
    (writeShellScriptBin "dotcontroller" "curl -s localhost:6033/graph | dot -Tpng | feh -")

    rain
    awscli2
    amazon-ecr-credential-helper
  ];

  programs.zsh.shellAliases = {
    "tf" = "terraform";
  };

  home.file.".docker/config.json".text = builtins.toJSON {
    credHelpers = {
      "305232526136.dkr.ecr.us-east-2.amazonaws.com" = "ecr-login";
      "public.ecr.aws" = "ecr-login";
    };
  };

  programs.zsh.functions."purge_deployment" = ''
    for key in $(http :8500/v1/kv/$1 keys==true | jq -r .'[]'); do http DELETE ":8500/v1/kv/$key"; done
  '';
}
