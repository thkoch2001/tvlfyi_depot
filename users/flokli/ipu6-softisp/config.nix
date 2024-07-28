{ pkgs, lib, ... }:

let
  libcamera = pkgs.libcamera.overrideAttrs (old: {
    mesonFlags = old.mesonFlags or [ ] ++ [
      "-Dpipelines=simple,ipu3,uvcvideo"
      "-Dipas=simple,ipu3"
    ];

    # This is
    # https://copr-dist-git.fedorainfracloud.org/cgit/jwrdegoede/ipu6-softisp/libcamera.git/plain/libcamera-0.2.0-softisp.patch?h=f39&id=60e6b3d5e366a360a75942073dc0d642e4900982,
    # but manually piped to git and back, as some renames were not processed properly.
    # It was later refreshed with https://patchwork.libcamera.org/cover/19663/
    patches = old.patches or [ ] ++ [
      ./libcamera/0001-libcamera-Add-support-for-IGIG_GBGR_IGIG_GRGB-bayer-.patch
      ./libcamera/0002-ov01a1s-HACK.patch
      ./libcamera/0003-libcamera-debayer_cpu-Make-the-minimum-size-1280x720.patch
    ];
  });

  # use patched libcamera
  pipewire' = (pkgs.pipewire.override {
    inherit libcamera;
  });

  wireplumber' = (pkgs.wireplumber.override {
    pipewire = pipewire';
  });
in
{
  boot.kernelPatches = [{
    name = "linux-kernel-test.patch";
    patch = ./kernel/softisp.patch;
    extraStructuredConfig = {
      # needed for /dev/dma_heap
      DMABUF_HEAPS_CMA = lib.kernel.yes;
      DMABUF_HEAPS_SYSTEM = lib.kernel.yes;
      DMABUF_HEAPS = lib.kernel.yes;
    };
  }];


  services.udev.extraRules = ''
    KERNEL=="system", SUBSYSTEM=="dma_heap", TAG+="uaccess"
  '';

  # provide qcam in $PATH.
  environment.systemPackages = [
    (libcamera.override {
      withQcam = true;
    })
  ];

  services.pipewire.package = pipewire';
  services.pipewire.wireplumber.package = wireplumber';
}
