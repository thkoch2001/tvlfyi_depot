# Configure TVL Keycloak instance.
#
# TODO(tazjin): Configure GitHub/GitLab IDP

terraform {
  required_providers {
    keycloak = {
      source = "mrparkers/keycloak"
    }
  }

  backend "s3" {
    endpoint = "https://objects.dc-sto1.glesys.net"
    bucket   = "tvl-state"
    key      = "terraform/tvl-keycloak"
    region   = "glesys"

    skip_credentials_validation = true
    skip_region_validation      = true
    skip_metadata_api_check     = true
  }
}

provider "keycloak" {
  client_id = "terraform"
  url       = "https://auth.tvl.fyi"
}

resource "keycloak_realm" "tvl" {
  realm                       = "TVL"
  enabled                     = true
  display_name                = "The Virus Lounge"
  default_signature_algorithm = "RS256"
}
