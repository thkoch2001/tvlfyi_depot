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
      # Whether to use nar-bridge to upload, rather than tvix-store copy.
      # using nar-bridge currently is "slower", as the `pkgs.mkBinaryCache` build
      # takes quite some time.
    , useNarBridge ? false

    , importPathName ? null

      # Commands to run before starting the tvix-daemon. Useful to provide
      # auxillary mock services.
    , preStart ? ""

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
        ] ++ lib.optionals (isClosure && useNarBridge) [
          depot.tvix.nar-bridge-go
          pkgs.curl
          pkgs.parallel
          pkgs.xz.bin
        ];
        buildCommand = ''
          touch $out
          # Ensure we can construct http clients.
          export SSL_CERT_FILE="${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt"

          ${preStart}

          # Start the tvix daemon, listening on a unix socket.
          BLOB_SERVICE_ADDR=${lib.escapeShellArg blobServiceAddr} \
          DIRECTORY_SERVICE_ADDR=${lib.escapeShellArg directoryServiceAddr} \
          PATH_INFO_SERVICE_ADDR=${lib.escapeShellArg pathInfoServiceAddr} \
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
        '' + lib.optionalString (isClosure && !useNarBridge) ''
          echo "Copying closure ${path}…"
          # This picks up the `closure` key in `$NIX_ATTRS_JSON_FILE` automatically.
          tvix-store --otlp=false copy
        '' + lib.optionalString (isClosure && useNarBridge) ''
          echo "Starting nar-bridge…"
          nar-bridge-http \
            --otlp=false \
            --store-addr=unix://$PWD/tvix-store.sock \
            --listen-addr=$PWD/nar-bridge.sock &

          # Wait for the socket to be created.
          while [ ! -e $PWD/nar-bridge.sock ]; do sleep 1; done

          # Upload. We can't use nix copy --to http://…, as it wants access to the nix db.
          # However, we can use mkBinaryCache to assemble .narinfo and .nar.xz to upload,
          # and then drive a HTTP client ourselves.
          to_upload=${pkgs.mkBinaryCache { rootPaths = [path];}}

          # Upload all NAR files (with some parallelism).
          # As mkBinaryCache produces them xz-compressed, unpack them on the fly.
          # nar-bridge doesn't care about the path we upload *to*, but a
          # subsequent .narinfo upload need to refer to its contents (by narhash).
          echo -e "Uploading NARs… "
          ls -d $to_upload/nar/*.nar.xz | parallel 'xz -d < {} | curl -s -T - --unix-socket $PWD/nar-bridge.sock http://localhost:9000/nar/$(basename {} | cut -d "." -f 1).nar'
          echo "Done."

          # Upload all NARInfo files.
          # FUTUREWORK: This doesn't upload them in order, and currently relies
          # on PathInfoService not doing any checking.
          # In the future, we might want to make this behaviour configurable,
          # and disable checking here, to keep the logic simple.
          ls -d $to_upload/*.narinfo | parallel 'curl -s -T - --unix-socket $PWD/nar-bridge.sock http://localhost:9000/$(basename {}) < {}'
        '' + ''
          # Invoke a VM using tvix as the backing store, ensure the outpath appears in its listing.
          echo "Starting VM…"

          CH_CMDLINE="${vmCmdline}" run-tvix-vm 2>&1 | tee output.txt
          grep "${assertVMOutput}" output.txt
        '';
        requiredSystemFeatures = [ "kvm" ];
      } // lib.optionalAttrs (isClosure && !useNarBridge) {
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
    blobServiceAddr = "objectstore+file:///build/blobs";
    directoryServiceAddr = "sled:///build/directories.sled";
    pathInfoServiceAddr = "sled:///build/pathinfo.sled";
    path = ../../docs;
    importPathName = "docs";
  });

  closure-tvix = (mkBootTest {
    blobServiceAddr = "objectstore+file:///build/blobs";
    path = depot.tvix.store;
    isClosure = true;
  });

  closure-nixos = (mkBootTest {
    blobServiceAddr = "objectstore+file:///build/blobs";
    path = testSystem;
    isClosure = true;
    vmCmdline = "init=${testSystem}/init panic=-1"; # reboot immediately on panic
    assertVMOutput = "Onwards and upwards.";
  });

  closure-nixos-nar-bridge = (mkBootTest {
    blobServiceAddr = "objectstore+file:///build/blobs";
    path = testSystem;
    useNarBridge = true;
    isClosure = true;
    vmCmdline = "init=${testSystem}/init panic=-1"; # reboot immediately on panic
    assertVMOutput = "Onwards and upwards.";
  });
}
