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

resource "buildkite_pipeline" "depot" {
  name        = "depot"
  description = "Run full CI pipeline of the depot, TVL's monorepo."
  repository  = "https://cl.tvl.fyi/depot"
  steps       = file("./steps-depot.yml")
}

resource "buildkite_pipeline" "tvl_kit" {
  name        = "tvl-kit"
  description = "TVL Kit, an exported subset of TVL depot"
  repository  = "https://code.tvl.fyi/depot.git:workspace=views/kit.git"
  steps       = file("./steps-tvl-kit.yml")
}
