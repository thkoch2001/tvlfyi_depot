# Configure TVL Keycloak instance.
#
# TODO(tazjin): Configure GitHub/GitLab IDP

terraform {
  required_providers {
    keycloak = {
      source = "mrparkers/keycloak"
    }
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

resource "keycloak_ldap_user_federation" "tvl_ldap" {
  name                    = "tvl-ldap"
  realm_id                = keycloak_realm.tvl.id
  enabled                 = true
  connection_url          = "ldap://localhost"
  users_dn                = "ou=users,dc=tvl,dc=fyi"
  username_ldap_attribute = "cn"
  uuid_ldap_attribute     = "cn"
  rdn_ldap_attribute      = "cn"
  full_sync_period        = 86400

  user_object_classes = [
    "inetOrgPerson",
    "organizationalPerson",
  ]
}
