# Buildkite configuration for TVL.

terraform {
  required_providers {
    buildkite = {
      source = "buildkite/buildkite"
    }
  }

  backend "s3" {
    endpoints = {
      s3 = "https://objects.dc-sto1.glesys.net"
    }
    bucket   = "tvl-state"
    key      = "terraform/tvl-buildkite"
    region   = "glesys"

    skip_credentials_validation = true
    skip_region_validation      = true
    skip_metadata_api_check     = true
    skip_requesting_account_id  = true
    skip_s3_checksum            = true
  }
}

provider "buildkite" {
  organization = "tvl"
}

resource "buildkite_pipeline" "depot" {
  name           = "depot"
  description    = "Run full CI pipeline of the depot, TVL's monorepo."
  repository     = "https://cl.tvl.fyi/depot"
  steps          = file("./steps-depot.yml")
  default_branch = "refs/heads/canon"
}

resource "buildkite_pipeline" "tvix" {
  name           = "tvix"
  description    = "Tvix, an exported subset of TVL depot"
  repository     = "https://code.tvl.fyi/depot.git:workspace=views/tvix.git"
  steps          = file("./steps-tvix.yml")
  default_branch = "canon"
}

resource "buildkite_pipeline" "tvl_kit" {
  name           = "tvl-kit"
  description    = "TVL Kit, an exported subset of TVL depot"
  repository     = "https://code.tvl.fyi/depot.git:workspace=views/kit.git"
  steps          = file("./steps-tvl-kit.yml")
  default_branch = "canon"
}
