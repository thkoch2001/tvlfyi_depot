{ depot, pkgs, lib, ... }:

let
  # Seed a tvix-store with the tvix docs, then start a VM, ask it to list all
  # files in /nix/store, and ensure the store path is present, which acts as a
  # nice smoketest.
  mkBootTest =
    { blobServiceAddr ? "memory://"
    , directoryServiceAddr ? "memory://"
    , pathInfoServiceAddr ? "memory://"


      # The path to import.
    , path

      # Whether the path should be imported as a closure.
      # If false, importPathName must be specified.
    , isClosure ? false
    , importPathName ? null

      # The cmdline to pass to the VM.
      # Defaults to tvix.find, which lists all files in the store.
    , vmCmdline ? "tvix.find"
      # The string we expect to find in the VM output.
      # Defaults the value of `path` (the store path we upload).
    , assertVMOutput ? path
    }:

      assert isClosure -> importPathName == null;
      assert (!isClosure) -> importPathName != null;

      pkgs.stdenv.mkDerivation ({
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
            tvix-store \
              --otlp=false \
              daemon -l $PWD/tvix-store.sock &

          # Wait for the socket to be created.
          while [ ! -e $PWD/tvix-store.sock ]; do sleep 1; done

          # Export env vars so that subsequent tvix-store commands will talk to
          # our tvix-store daemon over the unix socket.
          export BLOB_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.sock
          export DIRECTORY_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.sock
          export PATH_INFO_SERVICE_ADDR=grpc+unix://$PWD/tvix-store.sock
        '' + lib.optionalString (!isClosure) ''
          echo "Importing ${path} into tvix-store with name ${importPathName}…"
          cp -R ${path} ${importPathName}
          outpath=$(tvix-store import ${importPathName})

          echo "imported to $outpath"
        '' + lib.optionalString (isClosure) ''
          echo "Copying closure ${path}…"
          # This picks up the `closure` key in `$NIX_ATTRS_JSON_FILE` automatically.
          tvix-store --otlp=false copy
        '' + ''
          # Invoke a VM using tvix as the backing store, ensure the outpath appears in its listing.
          echo "Starting VM…"

          CH_CMDLINE="${vmCmdline}" run-tvix-vm 2>&1 | tee output.txt
          grep "${assertVMOutput}" output.txt
        '';
        requiredSystemFeatures = [ "kvm" ];
      } // lib.optionalAttrs (isClosure) {
        __structuredAttrs = true;
        exportReferencesGraph.closure = [ path ];
      });

  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;

  testSystem = systemFor ({ modulesPath, pkgs, ... }: {
    # Set some options necessary to evaluate.
    boot.loader.systemd-boot.enable = true;
    # TODO: figure out how to disable this without causing eval to fail
    fileSystems."/" = {
      device = "/dev/root";
      fsType = "tmpfs";
    };

    services.getty.helpLine = "Onwards and upwards.";
    systemd.services.do-shutdown = {
      after = [ "getty.target" ];
      description = "Shut down again";
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      script = "/run/current-system/sw/bin/systemctl poweroff --when=+10s";
    };

    # Don't warn about stateVersion.
    system.stateVersion = "24.05";
  });

in
depot.nix.readTree.drvTargets
{
  docs-memory = (mkBootTest {
    path = ../../docs;
    importPathName = "docs";
  });
  docs-persistent = (mkBootTest {
    blobServiceAddr = "objectstore+file://$PWD/blobs";
    directoryServiceAddr = "sled://$PWD/directories.sled";
    pathInfoServiceAddr = "sled://$PWD/pathinfo.sled";
    path = ../../docs;
    importPathName = "docs";
  });

  closure-tvix = (mkBootTest {
    blobServiceAddr = "objectstore+file://$PWD/blobs";
    path = depot.tvix.store;
    isClosure = true;
  });

  closure-nixos = (mkBootTest {
    blobServiceAddr = "objectstore+file://$PWD/blobs";
    path = testSystem;
    isClosure = true;
    vmCmdline = "init=${testSystem}/init panic=-1"; # reboot immediately on panic
    assertVMOutput = "Onwards and upwards.";
  });
}
