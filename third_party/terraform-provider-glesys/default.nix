# GleSYS Terraform provider
#
# Some TVL resources (DNS, object storage, ...) are hosted with them.
{ pkgs, ... }:

pkgs.terraform-providers.mkProvider rec {
  version = "0.3.2";

  owner = "glesys";
  repo = "terraform-provider-glesys";
  rev = "v${version}";
  sha256 = "1hlqa4f9d44hq614ff8ivg8a6fwg48jwz11zsrlghjzky82cfraq";

  vendorSha256 = "0g5g69absf0vmin0ff0anrxcgfq0bzx4iz3qci90p9xkvyph4nlw";

  # This provider is not officially published in the TF registry, so
  # we're giving it a fake source here.
  provider-source-address = "registry.terraform.io/depot/glesys";
}
