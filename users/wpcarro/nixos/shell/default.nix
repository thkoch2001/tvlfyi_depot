{ depot, pkgs, ... }:

# create Docker Image running NixOS with:
# - fish shell (with my prompt)
# - wpcarros-emacs
# - wpcarro.common.shell-utils

let
  inherit (depot.users) wpcarro;

  name = "wpcarros-shell";
  tag = "latest";
  port = "3000";

  wpcarrosEmacs = wpcarro.emacs.nixos {
    env = { "CI" = "true"; };
  };

  bins = wpcarro.common.shell-utils ++ (with pkgs; [
    busybox
    htop
    wpcarrosEmacs
  ]);

  # base this off of NixOS
  dockerImage = pkgs.dockerTools.buildLayeredImage {
    inherit name tag;
    config.fromImageName = "nixos/nix";
    config.Env = [ "PATH=${pkgs.lib.strings.makeBinPath bins}:$PATH" ];
    config.Cmd = [ "${pkgs.bash}/bin/bash" ];
    maxLayers = 120;
  };

in
pkgs.writeShellScriptBin "expose-shell" ''
  set -euo pipefail

  if [[ $EUID -ne 0 ]]; then
    echo "Oh no! Only root is allowed to run this!" >&2
    exit 1
  fi

  ${pkgs.docker}/bin/docker load < "${dockerImage}"
  ${pkgs.gotty}/bin/gotty --port ${port} -w ${pkgs.docker}/bin/docker run -ti --rm "${name}:${tag}" "${pkgs.bash}/bin/bash"
''
