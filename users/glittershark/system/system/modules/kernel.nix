{ config, lib, pkgs, ... }:
with lib.versions;
let
  inherit (pkgs) stdenvNoCC;
  kernelRelease = config.boot.kernelPackages.kernel.version;
  mj = major kernelRelease;
  mm = majorMinor kernelRelease;
  patched-linux-ck = stdenvNoCC.mkDerivation {
    name = "linux-ck";
    src = builtins.fetchurl {
      name = "linux-ck-patch-${mm}-ck1.xz";
      # example: http://ck.kolivas.org/patches/5.0/5.4/5.4-ck1/patch-5.4-ck1.xz
      url = "http://ck.kolivas.org/patches/${mj}.0/${mm}/${mm}-ck1/patch-${mm}-ck1.xz";
      sha256 = "1kka38rmjcqsv4j2anczrsni0bf6yfdx2vsxbna3ic84nh3rz434";
    };

    unpackPhase = ''
      ${pkgs.xz}/bin/unxz -kfdc $src > patch-${mm}-ck1
    '';

    installPhase = ''
      cp patch-${mm}-ck1 $out
    '';
  };
in
{
  boot.kernelPackages = pkgs.linuxPackages_5_10.extend (self: super: {
    kernel = super.kernel.override {
      ignoreConfigErrors = true;
      kernelPatches = super.kernel.kernelPatches ++ [{
        name = "linux-ck";
        patch = patched-linux-ck;
      }];
      argsOverride = {
        modDirVersion = super.kernel.modDirVersion + "-ck1";
      };
    };
  });
}
