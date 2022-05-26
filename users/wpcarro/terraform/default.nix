{ depot, pkgs, lib, ... }:

let
  inherit (builtins) concatLists concatStringsSep toJSON unsafeDiscardStringContext;
  inherit (depot.users) wpcarro;
  inherit (pkgs) writeText;

  images = import (pkgs.path + "/nixos/modules/virtualisation/gce-images.nix");
  nixosImage = images."20.09";
in
{
  googleCloudVM =
    { project
    , name
    , region
    , zone
    , configuration
    , extraConfig ? { }
    ,
    }:
    let
      inherit (configuration.users.users) root;
      inherit (configuration.networking) firewall;

      # Convert NixOS-style port numbers to Terraform-style.
      asStrings = xs: map toString xs;
      asRanges = xs: map (x: "${toString x.from}-${toString x.to}") xs;

      sshKeys = concatStringsSep "\n"
        (map (key: "root:${key}") root.openssh.authorizedKeys.keys);

      os = depot.ops.nixos.nixosFor (_: {
        imports = [
          (pkgs.path + "/nixos/modules/virtualisation/google-compute-image.nix")
          configuration
        ];

        networking.hostName = name;

        fileSystems."/nix" = {
          device = "/dev/disk/by-label/google-${name}-disk";
          fsType = "ext4";
        };
      });

      osRoot = os.config.system.build.toplevel;
      osPath = unsafeDiscardStringContext (toString osRoot.outPath);
      drvPath = unsafeDiscardStringContext (toString osRoot.drvPath);
    in
    {
      inherit drvPath osPath;
      json = writeText "terraform.tf.json" (toJSON (lib.recursiveUpdate extraConfig {
        provider.google = {
          inherit project region zone;
        };

        resource.google_compute_instance."${name}" = {
          inherit name zone;
          machine_type = "e2-standard-2";

          tags = [
            "http-server"
            "https-server"
            "${name}-firewall"
          ];

          boot_disk = {
            device_name = "boot";
            initialize_params = {
              size = 10;
              image = "projects/nixos-cloud/global/images/${nixosImage.name}";
            };
          };

          attached_disk = {
            source = "\${google_compute_disk.${name}.id}";
            device_name = "${name}-disk";
          };

          network_interface = {
            network = "default";
            subnetwork = "default";
            access_config = { };
          };

          # Copy root's SSH keys from the NixOS configuration and expose them to the
          # metadata server.
          metadata = {
            inherit sshKeys;
            ssh-keys = sshKeys;

            # NixOS's fetch-instance-ssh-keys.bash relies on these fields being
            # available on the metadata server.
            ssh_host_ed25519_key = "\${tls_private_key.${name}.private_key_pem}";
            ssh_host_ed25519_key_pub = "\${tls_private_key.${name}.public_key_pem}";

            # Even though we have SSH access, having oslogin can still be useful for
            # troubleshooting in the browser if for some reason SSH isn't working as
            # expected.
            enable-oslogin = "TRUE";
          };

          service_account.scopes = [ "cloud-platform" ];
        };

        resource.tls_private_key."${name}" = {
          algorithm = "ECDSA";
          ecdsa_curve = "P384";
        };

        resource.google_compute_firewall."${name}" = {
          name = "${name}-firewall";
          network = "default";

          # Read the firewall configuration from the NixOS configuration.
          allow = [
            {
              protocol = "tcp";
              ports = concatLists [
                (asStrings (firewall.allowedTCPPorts or [ ]))
                (asRanges (firewall.allowedTCPPortRanges or [ ]))
              ];
            }
            {
              protocol = "udp";
              ports = concatLists [
                (asStrings (firewall.allowedUDPPorts or [ ]))
                (asRanges (firewall.allowedUDPPortRanges or [ ]))
              ];
            }
          ];
          source_ranges = [ "0.0.0.0/0" ];
        };

        resource.google_compute_disk."${name}" = {
          inherit zone;
          name = "${name}-disk";
          size = 100;
        };

        resource.null_resource.deploy_nixos = {
          triggers = {
            # Redeploy when the NixOS configuration changes.
            os = "${osPath}";
            # Redeploy when a new machine is provisioned.
            machine_id = "\${google_compute_instance.${name}.id}";
          };

          connection = {
            host = "\${google_compute_instance.${name}.network_interface[0].access_config[0].nat_ip}";
          };

          provisioner = [
            { remote-exec.inline = [ "true" ]; }
            {
              local-exec.command = ''
                export PATH="${pkgs.openssh}/bin:$PATH"

                scratch="$(mktemp -d)"
                function cleanup() {
                  rm -rf $scratch
                }
                trap cleanup EXIT

                # write out ssh key
                echo -n "''${tls_private_key.${name}.private_key_pem}" > $scratch/id_rsa.pem
                chmod 0600 $scratch/id_rsa.pem

                export NIX_SSHOPTS="\
                  -o StrictHostKeyChecking=no\
                  -o UserKnownHostsFile=/dev/null\
                  -o GlobalKnownHostsFile=/dev/null\
                  -o IdentityFile=$scratch/id_rsa.pem
                "

                nix-build ${drvPath}
                nix-copy-closure --to \
                  root@''${google_compute_instance.${name}.network_interface[0].access_config[0].nat_ip} \
                  ${osPath} --gzip --use-substitutes
              '';
            }
            {
              remote-exec.inline = [
                "nix-env --profile /nix/var/nix/profiles/system --set ${osPath}"
                "${osPath}/bin/switch-to-configuration switch"
              ];
            }
          ];
        };
      }));
    };
}
