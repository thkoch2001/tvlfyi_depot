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
  # TODO: make a smaller kernel, we don't need a gazillion filesystems and
  # device drivers in it.
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
  uroot = pkgs.buildGoModule rec {
    pname = "u-root";
    version = "0.14.0";
    src = pkgs.fetchFromGitHub {
      owner = "u-root";
      repo = "u-root";
      rev = "v${version}";
      hash = "sha256-8zA3pHf45MdUcq/MA/mf0KCTxB1viHieU/oigYwIPgo=";
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

      GO111MODULE=off GOPATH=/tmp/go GOPROXY=off ${uroot}/bin/u-root -files ./tvix-init -initcmd "/tvix-init" -o $out
    '';
  };

  # Start a `tvix-store` virtiofs daemon from $PATH, then a cloud-hypervisor
  # pointed to it.
  # Supports the following env vars (and defaults)
  # CH_NUM_CPUS=2
  # CH_MEM_SIZE=512M
  # CH_CMDLINE=""
  runVM = pkgs.writers.writeBashBin "run-tvix-vm" ''
    tempdir=$(mktemp -d)

    cleanup() {
      kill $virtiofsd_pid
      if [[ -n ''${work_dir-} ]]; then
        chmod -R u+rw "$tempdir"
        rm -rf "$tempdir"
      fi
    }
    trap cleanup EXIT

    # Spin up the virtiofs daemon
    tvix-store --otlp=false virtiofs -l $tempdir/tvix.sock &
    virtiofsd_pid=$!

    # Wait for the socket to exist.
    until [ -e $tempdir/tvix.sock ]; do sleep 0.1; done

    CH_NUM_CPUS="''${CH_NUM_CPUS:-2}"
    CH_MEM_SIZE="''${CH_MEM_SIZE:-512M}"
    CH_CMDLINE="''${CH_CMDLINE:-}"

    # spin up cloud_hypervisor
    ${pkgs.cloud-hypervisor}/bin/cloud-hypervisor \
     --cpus boot=$CH_NUM_CPU \
     --memory mergeable=on,shared=on,size=$CH_MEM_SIZE \
     --console null \
     --serial tty \
     --kernel ${kernel}/${pkgs.stdenv.hostPlatform.linux-kernel.target} \
     --initramfs ${initrd} \
     --cmdline "console=ttyS0 $CH_CMDLINE" \
     --fs tag=tvix,socket=$tempdir/tvix.sock,num_queues=''${CH_NUM_CPU},queue_size=512
  '';

  meta.ci.targets = [
    "initrd"
    "kernel"
    "runVM"
  ];
}
