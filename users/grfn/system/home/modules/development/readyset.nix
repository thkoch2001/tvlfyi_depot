{ config, lib, pkgs, ... }:

{
  imports = [
    ./rust.nix
  ];

  home.packages = with pkgs; [
    # This goes in $PATH so I can run it from rofi and parent to my WM
    (writeShellScriptBin "dotclip" "xclip -out -selection clipboard | dot -Tpng | feh -")
    # https://buildkite.com/tvl/depot/builds/14577#0182449d-11fa-489a-a768-e7b004172c56
    #
    #   cft/build/build.go:38:10: fmt.Errorf format %w has arg r of wrong type interface{}
    #   cft/build/build.go:78:10: fmt.Errorf format %w has arg r of wrong type interface{}
    #   cft/build/build.go:167:10: fmt.Errorf format %w has arg r of wrong type interface{}
    #   FAIL    github.com/aws-cloudformation/rain/cft/build [build failed]
    #
    # (buildGoModule rec {
    #   pname = "rain";
    #   version = "1.2.0";

    #   src = fetchFromGitHub {
    #     owner = "aws-cloudformation";
    #     repo = pname;
    #     rev = "v${version}";
    #     sha256 = "168gkchshl5f1awqi1cgvdkm6q707702rnn0v4i5djqxmg5rk0p9";
    #   };

    #   vendorSha256 = "00bx7cjh5cq9zlis8lf28i016avgqf3j9fmcvkqzd8db2vxpqx3v";
    # })
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
