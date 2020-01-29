{
  pkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  ...
}:

pkgs.dockerTools.buildLayeredImage {
  name = "gemma";
  tag = "latest";
  config.ExposedPorts = {
    "4242" = {};
  };
  config.Env = [
    "GEMMA_CONFIG=${./config.lisp}"
  ];
  config.Cmd = [ "${depot.fun.gemma}/bin/gemma" ];
  maxLayers = 120;
}
