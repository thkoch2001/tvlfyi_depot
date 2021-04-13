# Configures an OpenLDAP instance for TVL
#
# TODO(tazjin): Configure ldaps://
{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  user = struct {
    username = string;
    email = string;
    password = string;
    displayName = option string;
  };

  toLdif = defun [ user string ] (u: ''
    dn: cn=${u.username},ou=users,dc=tvl,dc=fyi
    objectClass: organizationalPerson
    objectClass: inetOrgPerson
    sn: ${u.username}
    cn: ${u.username}
    displayName: ${u.displayName or u.username}
    mail: ${u.email}
    userPassword: ${u.password}
  '');

  inherit (depot.ops) users;

in {
  services.openldap = {
    enable = true;

    settings.children = {
      "olcDatabase={1}mdb".attrs = {
        objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
        olcDatabase = "{1}mdb";
        olcDbDirectory = "/var/lib/openldap";
        olcSuffix = "dc=tvl,dc=fyi";
        olcAccess = "to *  by * read";
        olcRootDN = "cn=admin,dc=tvl,dc=fyi";
        olcRootPW = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$OfcgkOQ96VQ3aJj7NfA9vQ$oS6HQOkYl/bUYg4SejpltQYy7kvqx/RUxvoR4zo1vXU";
      };

      "cn=module{0}".attrs = {
        objectClass = "olcModuleList";
        olcModuleLoad = "pw-argon2";
      };

      "cn=schema".includes =
        map (schema: "${pkgs.openldap}/etc/schema/${schema}.ldif")
            [ "core" "cosine" "inetorgperson" "nis" ];
    };

    # Contents are immutable at runtime, and adding user accounts etc.
    # is done statically in the LDIF-formatted contents in this folder.
    declarativeContents."dc=tvl,dc=fyi" = ''
      dn: dc=tvl,dc=fyi
      dc: tvl
      o: TVL LDAP server
      description: Root entry for tvl.fyi
      objectClass: top
      objectClass: dcObject
      objectClass: organization

      dn: ou=users,dc=tvl,dc=fyi
      ou: users
      description: All users in TVL
      objectClass: top
      objectClass: organizationalUnit

      dn: ou=groups,dc=tvl,dc=fyi
      ou: groups
      description: All groups in TVL
      objectClass: top
      objectClass: organizationalUnit

      ${lib.concatStringsSep "\n" (map toLdif users)}
    '';
  };
}
