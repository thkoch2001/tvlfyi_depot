Terraform for Keycloak
======================

This contains the Terraform configuration for deploying TVL's Keycloak
instance (which lives at `auth.tvl.fyi`).

Secrets are needed for applying this. The encrypted file
`//ops/secrets/tf-keycloak.age` contains `export` calls which should
be sourced, for example via `direnv`, by users with the appropriate
credentials.

An example `direnv` configuration used by tazjin is this:

```
# //ops/keycloak/.envrc
source_up
eval $(age --decrypt -i ~/.ssh/id_ed25519 $(git rev-parse --show-toplevel)/ops/secrets/tf-keycloak.age)
```
