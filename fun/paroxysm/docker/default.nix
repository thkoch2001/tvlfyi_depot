{ depot, ... }:

depot.third_party.dockerTools.buildLayeredImage {
  name = "tvldb";
  contents = [ depot.fun.tvldb ];
  config.Entrypoint = [ "${depot.fun.tvldb}/bin/paroxysm" ];
}
