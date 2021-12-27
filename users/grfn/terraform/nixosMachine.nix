{ depot, pkgs, lib, ... }:

# mostly stolen from espes

{ name
, instanceType
, configuration
, prefix ? "${name}_"
, region ? "us-east-2"
, rootVolumeSizeGb ? 50
, securityGroupId ? null
, extraIngressPorts ? []
}:

let
  os = depot.ops.nixos.nixosFor ({ modulesPath, ... }: {
    imports = [
      "${pkgs.path}/nixos/modules/virtualisation/amazon-image.nix"
      configuration
    ];

    ec2.hvm = true;
    networking.hostName = name;
    # TODO: remove this once the terraform tls provider supports ed25519 keys
    # https://github.com/hashicorp/terraform-provider-tls/issues/26
    services.openssh.extraConfig = ''
      PubkeyAcceptedKeyTypes=+ssh-rsa
      PubkeyAcceptedAlgorithms=+ssh-rsa
    '';
  });

  targetUser = "root";

  ec2Amis = import "${pkgs.path}/nixos/modules/virtualisation/ec2-amis.nix";

  osRoot = os.config.system.build.toplevel;

  osRootPath = builtins.unsafeDiscardStringContext (toString osRoot.outPath);
  drvPath = builtins.unsafeDiscardStringContext (toString osRoot.drvPath);

  machineResource = "aws_instance.${prefix}machine";

  recursiveMerge = builtins.foldl' lib.recursiveUpdate {};

  securityGroupId' =
    if isNull securityGroupId
    then "\${aws_security_group.${prefix}group.id}"
    else securityGroupId;
in recursiveMerge [
  (lib.optionalAttrs (isNull securityGroupId) {
    resource.aws_security_group."${prefix}group" = {
      provider = "aws.${region}";
      vpc_id = null;

      # terraform isn't good about knowing what other resources depend on
      # security groups
      lifecycle.create_before_destroy = true;
    };

    resource.aws_security_group_rule.all_egress = {
      provider = "aws.${region}";
      security_group_id = securityGroupId';
      type            = "egress";
      protocol        = "-1";
      from_port       = 0;
      to_port         = 0;
      cidr_blocks     = ["0.0.0.0/0"];
      ipv6_cidr_blocks = ["::/0"];

      description = null;
      prefix_list_ids = null;
      self = null;
    };
  })
  rec {
    data.external.my_ip = {
      program = [(pkgs.writeShellScript "my_ip" ''
        ${pkgs.jq}/bin/jq \
          -n \
          --arg ip "$(curl ifconfig.me)" \
          '{"ip":$ip}'
      '')];
    };

    resource.aws_security_group_rule.provision_ssh_access = {
      provider = "aws.${region}";
      security_group_id = securityGroupId';
      type = "ingress";
      protocol = "TCP";
      from_port = 22;
      to_port = 22;
      cidr_blocks = ["\${data.external.my_ip.result.ip}/32"];
      ipv6_cidr_blocks = [];
      description = null;
      prefix_list_ids = null;
      self = null;
    };

    resource.tls_private_key."${prefix}key" = {
      algorithm = "RSA";
    };

    resource.aws_key_pair."${prefix}generated_key" = {
      provider = "aws.${region}";
      key_name = "generated-key-\${sha256(tls_private_key.${prefix}key.public_key_openssh)}";
      public_key = "\${tls_private_key.${prefix}key.public_key_openssh}";
    };

    resource.aws_instance."${prefix}machine" = {
      provider = "aws.${region}";
      ami = ec2Amis."21.05"."${region}".hvm-ebs;
      instance_type = instanceType;
      vpc_security_group_ids = [ securityGroupId' ];
      key_name = "\${aws_key_pair.${prefix}generated_key.key_name}";
      root_block_device = {
        volume_size = rootVolumeSizeGb;
        tags.Name = name;
      };
      tags.Name = name;
    };

    resource.null_resource."${prefix}deploy_nixos" = {
      triggers = {
        # deploy if the machine is recreated
        machine_id = "\${${machineResource}.id}";

        # deploy on os changes
        os_drv = drvPath;
      };

      connection = {
        type = "ssh";
        host = "\${${machineResource}.public_ip}";
        user = targetUser;
        private_key = "\${tls_private_key.${prefix}key.private_key_pem}";
      };

      # do the actual deployment
      provisioner = [
        # wait till ssh is up
        { remote-exec.inline = [ "true" ]; }

        # copy the nixos closure
        {
          local-exec.command = ''
            export PATH="${pkgs.openssh}/bin:$PATH"

            scratch="$(mktemp -d)"
            trap 'rm -rf -- "$scratch"' EXIT

            # write out ssh key
            echo -n "''${tls_private_key.${prefix}key.private_key_pem}" > $scratch/id_rsa.pem
            chmod 0600 $scratch/id_rsa.pem

            export NIX_SSHOPTS="\
                -o StrictHostKeyChecking=no\
                -o UserKnownHostsFile=/dev/null\
                -o GlobalKnownHostsFile=/dev/null\
                -o IdentityFile=$scratch/id_rsa.pem"

            nix-build ${drvPath}
            nix-copy-closure \
              --to ${targetUser}@''${${machineResource}.public_ip} \
              ${osRootPath} \
              --gzip \
              --use-substitutes
          '';
        }

        # activate it
        {
          remote-exec.inline = [
            # semicolons mandatory
            ''
              set -e;
              nix-env --profile /nix/var/nix/profiles/system --set ${osRootPath};
              ${osRootPath}/bin/switch-to-configuration switch;
            ''
          ];
        }
      ];
    };
  }

  {
    resource.aws_security_group_rule = builtins.listToAttrs (map (port: {
      name = "ingress_${toString port}";
      value = {
        provider = "aws.${region}";
        security_group_id = securityGroupId';
        type = "ingress";
        protocol = "TCP";
        from_port = port;
        to_port = port;
        cidr_blocks = ["0.0.0.0/0"];
        ipv6_cidr_blocks = [];
        description = null;
        prefix_list_ids = null;
        self = null;
      };
    }) extraIngressPorts);
  }
]
