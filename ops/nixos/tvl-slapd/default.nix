# Configures an OpenLDAP instance for TVL
#
# TODO(tazjin): Configure ldaps://
{ config, lib, pkgs, ... }:

with config.depot.nix.yants;

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

  users = import ./users.nix;
in {
  # Use our patched OpenLDAP derivation which enables stronger password hashing.
  #
  # Unfortunately the module for OpenLDAP has no package option, so we
  # need to override it system-wide. Be aware that this triggers a
  # *large* number of rebuilds of packages such as GPG and Python.
  nixpkgs.overlays = [
    (_: _: {
      inherit (config.depot.third_party) openldap;
    })
  ];

  services.openldap = {
    enable = true;
    dataDir = "/var/lib/openldap";
    database = "mdb";
    suffix = "dc=tvl,dc=fyi";
    rootdn = "cn=admin,dc=tvl,dc=fyi";
    rootpw = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$OfcgkOQ96VQ3aJj7NfA9vQ$oS6HQOkYl/bUYg4SejpltQYy7kvqx/RUxvoR4zo1vXU";

    settings.children = {
      "olcDatabase={1}mdb".attrs = {
        objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
        olcDatabase = "{1}mdb";
        olcSuffix = "dc=tvl,dc=fyi";
        olcAccess = "to *  by * read";
      };

      "cn=module{0}".attrs = {
        objectClass = "olcModuleList";
        olcModuleLoad = "pw-argon2";
      };
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
