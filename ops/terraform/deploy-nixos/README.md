<!--
SPDX-FileCopyrightText: 2023 The TVL Authors

SPDX-License-Identifier: MIT
-->

deploy-nixos
============

This is a Terraform module to deploy a NixOS system closure to a
remote machine.

The system closure must be accessible by Nix-importing the repository
root and building a specific attribute
(e.g. `nix-build -A ops.machines.machine-name`).

The target machine must be accessible normally over SSH, and an SSH
key must be used for access.

Notably this module separates the evaluation of the system closure from building
and deploying it, and uses the closure's derivation hash to determine whether a
deploy is necessary.

## Usage example:

```terraform
module "deploy_somehost" {
  # Clone just this directory through josh. Add a `ref=` parameter to pin to a specific commit.
  source              = "git::https://code.tvl.fyi/depot.git:/ops/terraform/deploy-nixos.git"

  # The attribute.path pointing to the expression to instantiate.
  attrpath            = "ops.nixos.somehost"

  # The path to the Nix file to invoke. Optional.
  # If omitted, will shell out to git to determine the repo root, and Nix will
  # use `default.nix` in there.
  entrypoint          = "${path.module}/../../somewhere.nix"

  target_host         = "somehost.tvl.su"
  target_user         = "someone"
  target_user_ssh_key = tls_private_key.somehost.private_key_pem
}
```

## Future work

Several things can be improved about this module, for example:

* The remote system closure could be discovered to restore remote system state
  after manual deploys on the target (i.e. "stomping" of changes).

More ideas and contributions are, of course, welcome.

## Acknowledgements

Development of this module was sponsored by [Resoptima](https://resoptima.com/).
