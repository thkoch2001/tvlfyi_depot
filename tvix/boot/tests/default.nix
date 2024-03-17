{ depot, pkgs, lib, ... }:

let
  # Seed a tvix-store with the tvix docs, then start a VM, ask it to list all
  # files in /nix/store, and ensure the store path is present, which acts as a
  # nice smoketest.
  mkBootTest =
    { blobServiceAddr ? "memory://"
    , directoryServiceAddr ? "memory://"
    , pathInfoServiceAddr ? "memory://"
      # The path to import (via tvix-store import).
    , importPath ? ../../docs
    , importPathName ? "docs"
    }:
    pkgs.stdenv.mkDerivation {
      name = "run-vm";
      nativeBuildInputs = [
        depot.tvix.store
        depot.tvix.boot.runVM
      ];
      buildCommand = ''
        touch $out

        # Start the tvix daemon, listening on a unix socket.
        BLOB_SERVICE_ADDR=${blobServiceAddr} \
          DIRECTORY_SERVICE_ADDR=${directoryServiceAddr} \
          PATH_INFO_SERVICE_ADDR=${pathInfoServiceAddr} \
          tvix-store daemon -l $PWD/tvix-store.socket &

        # Wait for the socket to be created.
        while [ ! -e $PWD/tvix-store.socket ]; do sleep 1; done

        # Export env vars so that subsequent tvix-store commands will talk to
        # our tvix-store daemon over the unix socket.
        export BLOB_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.socket
        export DIRECTORY_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.socket
        export PATH_INFO_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.socket
      '' + lib.optionalString (importPath != null) ''
        echo "Importing ${importPath} into tvix-store with name ${importPathName}…"
        cp -R ${importPath} ${importPathName}
        outpath=$(tvix-store import ${importPathName})

        echo "imported to $outpath"

        # Invoke a VM using tvix as the backing store, ensure the outpath appears in its listing.

        CH_CMDLINE="tvix.find" run-tvix-vm 2>&1 | tee output.txt
        grep $outpath output.txt
      '';
      requiredSystemFeatures = [ "kvm" ];
    };
in
depot.nix.readTree.drvTargets {
  docs-memory = (mkBootTest { });
  docs-sled = (mkBootTest {
    blobServiceAddr = "sled://$PWD/blobs.sled";
    directoryServiceAddr = "sled://$PWD/directories.sled";
    pathInfoServiceAddr = "sled://$PWD/pathinfo.sled";
  });
  docs-objectstore-local = (mkBootTest {
    blobServiceAddr = "objectstore+file://$PWD/blobs";
  });
}
