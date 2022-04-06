{ config, lib, pkgs, ... }:

{
  imports = [
    ./rust.nix
  ];

  home.packages = with pkgs; [
    # This goes in $PATH so I can run it from rofi and parent to my WM
    (writeShellScriptBin "dotclip" "xclip -out -selection clipboard | dot -Tpng | feh -")
    (buildGoModule rec {
      pname = "rain";
      version = "1.2.0";

      src = fetchFromGitHub {
        owner = "aws-cloudformation";
        repo = pname;
        rev = "v${version}";
        sha256 = "168gkchshl5f1awqi1cgvdkm6q707702rnn0v4i5djqxmg5rk0p9";
      };

      vendorSha256 = "16bx7cjh5cq9zlis8lf28i016avgqf3j9fmcvkqzd8db2vxpqx3v";
    })
    awscli2
    amazon-ecr-credential-helper
  ];

  programs.zsh.shellAliases = {
    "tf" = "terraform";
  };
}
