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
      sha256 = "0cv1ayj9akl83q2whabj8v3qygkkfwvzcjqx539sw6j3r9qhrs64";
    };

    unpackPhase = ''
      ${pkgs.xz}/bin/unxz -kfdc $src > patch-${mm}-ck1
    '';

    patches = [
      (builtins.fetchurl {
        url = "https://aur.archlinux.org/cgit/aur.git/plain/fix_ck1_for_5.7.14.patch\?h\=linux-ck";
        sha256 = "0l8f2kph4f2lvcjn0s2fg6n9xa6f4khjz7rqc4zxk58r7fh4s5v4";
      })
    ];

    installPhase = ''
      cp patch-${mm}-ck1 $out
    '';
  };
in
{
  boot.kernelPackages = pkgs.linuxPackages_latest.extend (self: super: {
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
