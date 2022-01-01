# All user sources, that is services from which Keycloak gets user
# information (either by accessing a system like LDAP or integration
# through protocols like OIDC).

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
