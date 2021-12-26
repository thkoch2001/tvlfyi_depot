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
  trust_email             = true

  user_object_classes = [
    "inetOrgPerson",
    "organizationalPerson",
  ]
}

resource "keycloak_openid_client" "oauth2_proxy" {
  realm_id              = keycloak_realm.tvl.id
  client_id             = "oauth2-proxy"
  name                  = "TVL OAuth2 Proxy"
  enabled               = true
  access_type           = "CONFIDENTIAL"
  standard_flow_enabled = true

  valid_redirect_uris = [
    "https://login.tvl.fyi/oauth2/callback",
    "http://localhost:4774/oauth2/callback",
  ]
}

resource "keycloak_openid_audience_protocol_mapper" "oauth2_proxy_audience" {
  realm_id                 = keycloak_realm.tvl.id
  client_id                = keycloak_openid_client.oauth2_proxy.id
  name                     = "oauth2-proxy-audience"
  included_custom_audience = keycloak_openid_client.oauth2_proxy.client_id
}
