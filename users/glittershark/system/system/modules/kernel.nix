{ config, lib, pkgs, ... }:
with lib.versions;
let
  inherit (pkgs) runCommand;
  kernelRelease = config.linuxPackages.kernel.version or pkgs.linux.version;
  mj = major kernelRelease;
  mm = majorMinor kernelRelease;
  linux-ck = runCommand "linux-ck-combined.patch" {} ''
    ${pkgs.xz}/bin/unxz -kfdc ${builtins.fetchurl {
      # http://ck.kolivas.org/patches/5.0/5.4/5.4-ck1/patch-5.4-ck1.xz
      url = "http://ck.kolivas.org/patches/${mj}.0/${mm}/${mm}-ck1/patch-${mm}-ck1.xz";
      sha256 = "0p2ccwlsmq0587x6cnbrk4h2bwpl9342bmhsbyi1a87cs2jfwigl";
    }} > $out
  '';
in
{
  boot.kernelPackages = pkgs.linuxPackages.extend (self: super: {
    kernel = super.kernel.override {
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
