# This module deploys a NixOS host by building a system closure
# located at the specified attribute in the current repository.
#
# The closure's derivation path is persisted in the Terraform state to
# determine after Nix evaluation whether the system closure has
# changed and needs to be built/deployed.
#
# The system configuration is then built (or substituted) on the
# machine that runs `terraform apply`, then copied and activated on
# the target machine using `nix-copy-closure`.

variable "closure" {
  description = "attribute set path pointing to the NixOS system closure"
  type        = string
}

variable "entrypoint" {
  description = <<EOT
    Path to a .nix file (or directory containing `default.nix` file)
    that provides the attrset specified in `closure`.
    If unset, asks git for the root of the repository.
  EOT
  type        = string
  default     = ""
}

variable "target_address" {
  description = "address (IP or hostname) at which the target is reachable"
  type        = string
}

variable "target_user" {
  description = "username on the target machine"
  type        = string
}

variable "target_user_key" {
  description = "SSH key to use for connecting to the target"
  type        = string
  sensitive   = true
}

variable "triggers" {
  type        = map(string)
  description = "Triggers for deploy"
  default     = {}
}

# Fetch the derivation hash for the NixOS system.
data "external" "nixos_system" {
  program = ["${path.module}/nixos-eval.sh"]

  query = {
    closure    = var.closure
    entrypoint = var.entrypoint
  }
}

# Deploy the NixOS configuration if anything changed.
resource "null_resource" "nixos_deploy" {
  connection {
    type        = "ssh"
    host        = var.target_address
    user        = var.target_user
    private_key = var.target_user_key
  }

  # 1. Wait for SSH to become available.
  provisioner "remote-exec" {
    inline = ["true"]
  }

  # 2. Build NixOS system.
  provisioner "local-exec" {
    command = "nix-build ${data.external.nixos_system.result.drv} --no-out-link"
  }

  # 3. Copy closure to the target.
  provisioner "local-exec" {
    command = "${path.module}/nixos-copy.sh"

    environment = {
      SYSTEM_DRV     = data.external.nixos_system.result.drv
      TARGET_ADDRESS = var.target_address
      DEPLOY_KEY     = var.target_user_key
      TARGET_USER    = var.target_user
    }
  }

  # 4. Activate closure on the target.
  provisioner "remote-exec" {
    inline = [
      "set -eu",
      "SYSTEM=$(nix-build ${data.external.nixos_system.result.drv} --no-out-link)",
      "sudo nix-env --profile /nix/var/nix/profiles/system --set $SYSTEM",
      "sudo $SYSTEM/bin/switch-to-configuration switch",
    ]
  }

  triggers = merge({
    nixos_drv      = data.external.nixos_system.result.drv
    closure        = var.closure
    target_address = var.target_address
  }, var.triggers)
}

output "nixos_drv" {
  value = data.external.nixos_system.result
}
