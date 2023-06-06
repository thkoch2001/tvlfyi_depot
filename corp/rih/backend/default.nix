{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  binary = depot.third_party.naersk.buildPackage {
    src = ./.;
  };

  image = pkgs.dockerTools.buildLayeredImage {
    name = "rih-backend";
    config.Cmd = [ "${binary}/bin/rih-backend" ];

    contents = [
      binary
    ];
  };
}
