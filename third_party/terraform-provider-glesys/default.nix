# GleSYS Terraform provider
#
# Some TVL resources (DNS, object storage, ...) are hosted with them.
{ pkgs, ... }:

pkgs.terraform-providers.mkProvider rec {
  version = "0.9.0";
  spdx = "MPL-2.0";

  owner = "glesys";
  repo = "terraform-provider-glesys";
  rev = "v${version}";
  hash = "sha256:0n2wb1gl0agc9agqlmhg4mh9kyfhw4zvrryyl8wfxlp1hkr0wz9y";

  vendorHash = "sha256:13wdx7q5rsyjrm6cn030m5hgcvx0m17dhr16wmbfv71pmsszfdjm";

  # This provider is not officially published in the TF registry, so
  # we're giving it a fake source here.
  provider-source-address = "registry.terraform.io/depot/glesys";
}
