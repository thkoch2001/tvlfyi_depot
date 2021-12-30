# diogenes

diogenes is a NixOS machine deployed on a Google VM. It hosts
https://wpcarro.dev.

## Deployment

I manage diogenes's deployment with Terraform. My current workflow looks like
this (highly subject to change):

```shell
cd /tmp/terraform # or any directory that hosts terraform state
outpath=$(nix-build /depot -A users.wpcarro.nixos.diogenes)
cp <out-path> .
nix-shell -p terraform google-cloud-sdk # gcloud to authenticate if necessary
terraform init/apply
```
