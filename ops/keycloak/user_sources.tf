# All user sources, that is services from which Keycloak gets user
# information (either by accessing a system like LDAP or integration
# through protocols like OIDC).

variable "github_client_secret" {
  type = string
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

  lifecycle {
    # Without this, terraform wants to recreate the resource.
    ignore_changes = [
      delete_default_mappers
    ]
  }
}

# keycloak_oidc_identity_provider.github will be destroyed
# (because keycloak_oidc_identity_provider.github is not in configuration)
resource "keycloak_oidc_identity_provider" "github" {
  alias                 = "github"
  provider_id           = "github"
  client_id             = "6d7f8bb2e82bb6739556"
  client_secret         = var.github_client_secret
  realm                 = keycloak_realm.tvl.id
  backchannel_supported = false
  gui_order             = "1"
  store_token           = false
  sync_mode             = "IMPORT"
  trust_email           = true

  # These default to built-in values for the `github` provider_id.
  authorization_url = ""
  token_url         = ""
}
