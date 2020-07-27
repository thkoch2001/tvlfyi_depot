{ depot, ... }:

depot.third_party.dockerTools.buildLayeredImage {
  name = "paroxysm";
  contents = [ depot.fun.paroxysm ];
  config.Entrypoint = [ "${depot.fun.paroxysm}/bin/paroxysm" ];
}
