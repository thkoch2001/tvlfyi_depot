{ depot, pkgs, ... }:

pkgs.dockerTools.buildLayeredImage {
  name = "tvldb";
  contents = [ depot.fun.tvldb ];
  config.Entrypoint = [ "${depot.fun.tvldb}/bin/paroxysm" ];
}
