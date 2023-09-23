{ depot, pkgs, ... }:

rec {
  fedoraCloudImage = pkgs.fetchurl {
    url = "https://download.fedoraproject.org/pub/fedora/linux/releases/38/Cloud/x86_64/images/Fedora-Cloud-Base-38-1.6.x86_64.qcow2";
    hash = "sha256-0zRnBAH/PVtBKfzGYs9k9ablaCKK9ZB2zESaSUUxhII=";
  };
  # This is used to configure cloud-init, building an ISO image which is attached to the VM
  # It mounts the tvix store to /nix/store, lists all contents, then powers off the VM.
  metadataDrive = pkgs.stdenv.mkDerivation {
    name = "cidata.iso";
    buildCommand = ''
      cat << EOF > user-data
      #cloud-config
      bootcmd:
       - ["mkdir", "-p", "/nix/store"]
      mounts:
       - ['tvix', '/nix/store', 'virtiofs', 'ro']

      runcmd:
       - ["find", "/nix/store"]
       - ["systemctl", "poweroff"]
      EOF

      touch meta-data

      ${pkgs.cdrkit}/bin/genisoimage --output $out -volid cidata -joliet -rock user-data meta-data
    '';
  };

  # Seed a tvix-store with the tvix docs, start virtiofs backend and spin up qemu.
  # Then search for the store path in the output.
  runVM = pkgs.stdenv.mkDerivation {
    name = "run-vm";
    nativeBuildInputs = [ pkgs.qemu_kvm ];
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
      ${depot.tvix.store}/bin/tvix-store virtio-fs -l /tmp/tvix.sock &

      # Wait for the socket to exist. TODO: Make this more reliable
      sleep 2

      # Copy the hard drive and make it mutable.
      cp ${fedoraCloudImage} hdd.qcow2
      chmod +w hdd.qcow2
      
      qemu-kvm \
        -drive file=hdd.qcow2 \
        -cdrom ${metadataDrive} \
        -chardev socket,id=char0,path=/tmp/tvix.sock \
        -device vhost-user-fs-pci,queue-size=1024,chardev=char0,tag=tvix \
        -m 1G \
        -object memory-backend-file,id=mem,size=1G,mem-path=/dev/shm,share=on \
        -numa node,memdev=mem \
        -nographic 2>&1 | tee output.txt

      grep ${../docs} output.txt
    '';

    requiredSystemFeatures = [ "kvm" ];
  };
}
