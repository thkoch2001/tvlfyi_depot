{ depot, lib, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  # Provide a Terraform wrapper with Yandex Cloud support.
  terraform = pkgs.terraform.withPlugins (p: [
    p.yandex
  ]);

  validate = depot.tools.checks.validateTerraform {
    inherit terraform;
    name = "corp";
    src = lib.cleanSource ./.;
  };

  # Yandex Cloud CLI
  yc-cli = pkgs.stdenv.mkDerivation rec {
    pname = "yc-cli";
    version = "0.106.0";

    src = pkgs.fetchurl {
      url = "https://storage.yandexcloud.net/yandexcloud-yc/release/${version}/linux/amd64/yc";
      sha256 = "sha256:1f7fq9rlihz91ld1vdjj9vq9ssq1ls031jin4zisxv75rcdpslh3";
    };

    phases = [ "installPhase" ];
    installPhase = "install -D $src $out/bin/yc";
  };

  deps = depot.tools.depot-deps.overrideDeps {
    tf-yandex = {
      attr = "corp.ops.terraform";
      cmd = "terraform";
    };

    yc.attr = "corp.ops.yc-cli";
  };
}
