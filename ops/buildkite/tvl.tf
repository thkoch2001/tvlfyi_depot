# Buildkite configuration for TVL.

terraform {
  required_providers {
    buildkite = {
      source = "buildkite/buildkite"
    }
  }

  backend "s3" {
    endpoint = "https://objects.dc-sto1.glesys.net"
    bucket   = "tvl-state"
    key      = "terraform/tvl-buildkite"
    region   = "glesys"

    skip_credentials_validation = true
    skip_region_validation      = true
    skip_metadata_api_check     = true
  }
}

provider "buildkite" {
  organization = "tvl"
}
