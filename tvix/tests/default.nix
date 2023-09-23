{ depot, pkgs, ... }:

rec {
  # A binary that sets up /nix/store from virtiofs, lists all store paths, and
  # powers off the machine.
  tvix-init = depot.nix.buildGo.program {
    name = "tvix-init";
    srcs = [
      ./tvix-init.go
    ];
  };

  # A kernel with virtiofs support baked in
  kernel = pkgs.buildLinux ({ } // {
    inherit (pkgs.linuxPackages_latest.kernel) src version modDirVersion;
    autoModules = false;
    kernelPreferBuiltin = true;
    ignoreConfigErrors = true;
    kernelPatches = [ ];
    structuredExtraConfig = with pkgs.lib.kernel; {
      FUSE_FS = option yes;
      DAX_DRIVER = option yes;
      DAX = option yes;
      FS_DAX = option yes;
      VIRTIO_FS = option yes;
      VIRTIO = option yes;
      ZONE_DEVICE = option yes;
    };
  });

  # A build framework for minimal initrds
  uroot = pkgs.buildGoModule {
    pname = "u-root";
    version = "unstable-2023-09-20";
    src = pkgs.fetchFromGitHub {
      owner = "u-root";
      repo = "u-root";
      rev = "72921548ce2e88c4c5b62e83c717cbd834b58067";
      hash = "sha256-fEoUGqh6ZXprtSpJ55MeuSFe7L5A/rkIIVLCwxbPHzE=";
    };
    vendorHash = null;

    doCheck = false; # Some tests invoke /bin/bash
  };

  # Use u-root to build a initrd with our tvix-init inside.
  initrd = pkgs.stdenv.mkDerivation {
    name = "initrd.cpio";
    nativeBuildInputs = [ pkgs.go ];
    # https://github.com/u-root/u-root/issues/2466
    buildCommand = ''
      mkdir -p /tmp/go/src/github.com/u-root/
      cp -R ${uroot.src} /tmp/go/src/github.com/u-root/u-root
      cd /tmp/go/src/github.com/u-root/u-root
      chmod +w .
      cp ${tvix-init}/bin/tvix-init tvix-init

      export HOME=$(mktemp -d)
      export GOROOT="$(go env GOROOT)"
      
      GO111MODULE=off GOPATH=/tmp/go GOPROXY=off ${uroot}/bin/u-root -files ./tvix-init -uinitcmd "/tvix-init" -o $out
    '';
  };

  # Seed a tvix-store with the tvix docs, start virtiofs backend and spin up qemu.
  # Then search for the store path in the output.
  runVM = pkgs.stdenv.mkDerivation {
    name = "run-vm";
    nativeBuildInputs = [ pkgs.cloud-hypervisor ];
    buildCommand = ''
      touch $out

      # Configure tvix to put data in the local working directory
      export BLOB_SERVICE_ADDR=sled://$PWD/blobs.sled
      export DIRECTORY_SERVICE_ADDR=sled://$PWD/directories.sled
      export PATH_INFO_SERVICE_ADDR=sled://$PWD/pathinfo.sled

      # Seed the tvix store with some data
      # Create a `docs` directory with the contents from ../docs
      # Make sure it still is called "docs" when calling import, so we can
      # predict the store path.
      cp -R ${../docs} docs
      outpath=$(${depot.tvix.store}/bin/tvix-store import docs)
      
      echo "Store contents imported to $outpath"

      # Spin up the virtiofs daemon
      ${depot.tvix.store}/bin/tvix-store virtiofs -l /tmp/tvix.sock &

      # Wait for the socket to exist. TODO: Make this more reliable
      sleep 2

      cloud-hypervisor \
       --cpus boot=1 \
       --console null \
       --serial tty \
       --kernel ${kernel.dev}/vmlinux \
       --initramfs ${initrd} \
       --cmdline "console=ttyS0" \
       --memory mergeable=on,shared=on,size=512M \
       --fs tag=tvix,socket=/tmp/tvix.sock,num_queues=1,queue_size=512 2>&1 | tee output.txt

      grep ${../docs} output.txt
    '';
    requiredSystemFeatures = [ "kvm" ];
  };

  meta.ci.targets = [ "runVM" ];
}
