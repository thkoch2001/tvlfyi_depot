Buildkite configuration
=======================

This contains Terraform configuration for setting up our Buildkite
pipelines.

Each pipeline (such as the one for depot itself, or exported subsets
of the depot) needs some static configuration stored in Buildkite.

Through `//tools/depot-deps` a `tf-buildkite` binary is made available
which contains a Terraform binary pre-configured with the correct
providers. This is automatically on your `$PATH` through `direnv`.

However, secrets still need to be loaded to access the Terraform state
and speak to the Buildkite API. These are available to certain users
through `//ops/secrets`.

This can be done with separate direnv configuration, for example:

```
# //ops/buildkite/.envrc
source_up
eval $(age --decrypt -i ~/.ssh/id_ed25519 $(git rev-parse --show-toplevel)/ops/secrets/tf-buildkite.age)
```
