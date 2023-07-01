# All Keycloak clients, that is applications which authenticate
# through Keycloak.
#
# Includes first-party (i.e. TVL-hosted) and third-party clients.

resource "keycloak_openid_client" "grafana" {
  realm_id              = keycloak_realm.tvl.id
  client_id             = "grafana"
  name                  = "Grafana"
  enabled               = true
  access_type           = "CONFIDENTIAL"
  standard_flow_enabled = true
  base_url              = "https://status.tvl.su"

  valid_redirect_uris = [
    "https://status.tvl.su/*",
  ]
}

resource "keycloak_openid_client" "gerrit" {
  realm_id                                 = keycloak_realm.tvl.id
  client_id                                = "gerrit"
  name                                     = "TVL Gerrit"
  enabled                                  = true
  access_type                              = "CONFIDENTIAL"
  standard_flow_enabled                    = true
  base_url                                 = "https://cl.tvl.fyi"
  description                              = "TVL's code review tool"
  direct_access_grants_enabled             = true
  exclude_session_state_from_auth_response = false

  valid_redirect_uris = [
    "https://cl.tvl.fyi/*",
  ]

  web_origins = [
    "https://cl.tvl.fyi",
  ]
}

resource "keycloak_saml_client" "buildkite" {
  realm_id  = keycloak_realm.tvl.id
  client_id = "https://buildkite.com"
  name      = "Buildkite"
  base_url  = "https://buildkite.com/sso/tvl"

  client_signature_required   = false
  assertion_consumer_post_url = "https://buildkite.com/sso/~/1531aca5-f49c-4151-8832-a451e758af4c/saml/consume"

  valid_redirect_uris = [
    "https://buildkite.com/sso/~/1531aca5-f49c-4151-8832-a451e758af4c/saml/consume"
  ]
}

resource "keycloak_saml_user_attribute_protocol_mapper" "buildkite_email" {
  realm_id                   = keycloak_realm.tvl.id
  client_id                  = keycloak_saml_client.buildkite.id
  name                       = "buildkite-email-mapper"
  user_attribute             = "email"
  saml_attribute_name        = "email"
  saml_attribute_name_format = "Unspecified"
}

resource "keycloak_saml_user_attribute_protocol_mapper" "buildkite_name" {
  realm_id                   = keycloak_realm.tvl.id
  client_id                  = keycloak_saml_client.buildkite.id
  name                       = "buildkite-name-mapper"
  user_attribute             = "displayName"
  saml_attribute_name        = "name"
  saml_attribute_name_format = "Unspecified"
}

resource "keycloak_openid_client" "panettone" {
  realm_id              = keycloak_realm.tvl.id
  client_id             = "panettone"
  name                  = "Panettone"
  enabled               = true
  access_type           = "CONFIDENTIAL"
  standard_flow_enabled = true

  valid_redirect_uris = [
    "https://b.tvl.fyi/auth",
    "http://localhost:6161/auth",
  ]
}
