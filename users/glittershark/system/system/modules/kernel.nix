{ config, lib, pkgs, ... }:
with lib.versions;
let
  inherit (pkgs) runCommand;
  kernelRelease = config.boot.kernelPackages.kernel.version;
  mj = major kernelRelease;
  mm = majorMinor kernelRelease;
  linux-ck = runCommand "linux-ck-combined.patch" {} ''
    ${pkgs.xz}/bin/unxz -kfdc ${builtins.fetchurl {
      # example: http://ck.kolivas.org/patches/5.0/5.4/5.4-ck1/patch-5.4-ck1.xz
      url = "http://ck.kolivas.org/patches/${mj}.0/${mm}/${mm}-ck1/patch-${mm}-ck1.xz";
      sha256 = "01jyg9x2ligr0gjic8lg4f7hw3isz94kqwdbzdk9n8nghklh38p4";
    }} > $out
  '';
in
{
  boot.kernelPackages = pkgs.linuxPackages_latest.extend (self: super: {
    kernel = super.kernel.override {
      ignoreConfigErrors = true;
      kernelPatches = super.kernel.kernelPatches ++ [{
        name = "linux-ck";
        patch = linux-ck;
      }];
      argsOverride = {
        modDirVersion = super.kernel.modDirVersion + "-ck1";
      };
    };
  });
}
