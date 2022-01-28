# GleSYS Terraform provider
#
# Some TVL resources (DNS, object storage, ...) are hosted with them.
{ pkgs
, ...
}:
pkgs.terraform-providers.mkProvider
  rec
    {
    version = "0.3.1";
    owner = "glesys";
    repo = "terraform-provider-glesys";
    rev = "v${ version }";
    sha256 = "1rcwzf31gdxjywkcnlq1nxv4y8fcrc2z2xrp73q61mglv01bqq8m";
    vendorSha256 = "0g5g69absf0vmin0ff0anrxcgfq0bzx4iz3qci90p9xkvyph4nlw";
    # This provider is not officially published in the TF registry, so
    # we're giving it a fake source here.
    provider-source-address = "registry.terraform.io/depot/glesys";
  }
