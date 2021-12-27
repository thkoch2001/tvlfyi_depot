Terraform for GleSYS
======================

This contains the Terraform configuration for deploying TVL's
infrastructure at [GleSYS](https://glesys.com). This includes object
storage (e.g. for backups and Terraform state) and DNS.

Secrets are needed for applying this. The encrypted file
`//ops/secrets/tf-glesys.age` contains `export` calls which should be
sourced, for example via `direnv`, by users with the appropriate
credentials.

An example `direnv` configuration used by tazjin is this:

```
# //ops/secrets/.envrc
source_up
eval $(age --decrypt -i ~/.ssh/id_ed25519 $(git rev-parse --show-toplevel)/ops/secrets/tf-glesys.age)
watch_file $(git rev-parse --show-toplevel)/secrets/tf-glesys.age
```
