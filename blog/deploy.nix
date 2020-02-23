{ pkgs, briefcase, ... }:

pkgs.dockerTools.buildLayeredImage {
  name = "blog";
  tag = "latest";
  config.ExposedPorts = {
    "4242" = {};
  };
  config.Cmd = [ "${briefcase.blog}/bin/server" ];
  maxLayers = 120;
}
