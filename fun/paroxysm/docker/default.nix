{ depot, pkgs, ... }:

pkgs.pkgsBuildHost.dockerTools.buildLayeredImage {
  name = "paroxysm";
  contents = [ depot.fun.paroxysm ];
  config.Entrypoint = [ "${depot.fun.paroxysm}/bin/paroxysm" ];
}
