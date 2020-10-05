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

  users = [
    {
      username = "andi";
      email = "andi@notmuch.email";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$8lefg7+8UPAEh9Ott8zH0A$7YuLRraTC1IgxTNTxFJF03AWmqBS3GX2+vfD4XVTrb0";
    }
    {
      username = "artemist";
      email = "me@artem.ist";
      password = "{SSHA}N6Tl/txGQwlmVa7xVJCXpGcD1U4bJaI+";
    }
    {
      username = "camsbury";
      email = "camsbury7@gmail.com";
      password = "{SSHA}r6/I/zefrAb1jWTdhuqWik0CXT8E+/E5";
    }
    {
      username = "cynthia";
      email = "cynthia@tvl.fyi";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=4,p=1$TxjbMGenhEmkyYLrg5uGhbr60THB86YeRZg5bPdiTJo$k9gbRlAPjmxwdUwzbavvsAVkckgQZ0jS2oTtvZBPysk";
    }
    {
      username = "edef";
      email = "edef@edef.eu";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$OORx4ERbkgvTmuYCJA8cIw$i5qaBzHkRVw7Tl+wZsTFTDqJwF0vuZqhW3VpknMYMc0";
    }
    {
      username = "ericvolp12";
      email = "ericvolp12@gmail.com";
      password = "{SSHA}pSepaQ+/5KBLfJtRR5rfxGU8goAsXgvk";
    }
    {
      username = "eta";
      email = "eta@theta.eu.org";
      password = "{SSHA}sOR5xzi7Lfv376XGQA8Hf6jyhTvo0XYc";
    }
    {
      username = "firefly";
      email = "firefly@firefly.nu";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$RYVVkFoi3A1yYkI8J2zUwg$GUERvgHvU8SGjQmilDJGZu50hYRAHw+ejtuL+Skygs8";
    }
    {
      username = "glittershark";
      email = "grfn@gws.fyi";
      password = "{SSHA}i7PSAsXwJT3jjmmvU77aar/tU/YPDCEO";
    }
    {
      username = "isomer";
      email = "isomer@tvl.fyi";
      password = "{SSHA}OhWQkPJgH1rRJqYIaMUbbKC4iLEzvCev";
    }
    {
      username = "lukegb";
      email = "lukegb@tvl.fyi";
      password = "{SSHA}7a85VNhpFElFw+N5xcjgGmt4HnBsaGp4";
    }
    {
      username = "multi";
      email = "depot@in-addr.xyz";
      password = "{ARGON2}$argon2i$v=19$m=4096,t=3,p=1$qCfXhZUVft1YVPx7H4x7rw$dhtwtCrEMSpZfWQJbw2wpo5XHqiJqoZkiKeEbE6AdX0";
    }
    {
      username = "nyanotech";
      email = "nyanotechnology@gmail.com";
      password = "{SSHA}NIJ2RCRb1+Q4Bs63cyE91VZyiN47DG6y";
    }
    {
      username = "Profpatsch";
      email = "mail@profpatsch.de";
      password = "{SSHA}jcFXxRplMFxH4gpa0X5VdUzW64T95TwQ";
    }
    {
      username = "q3k";
      email = "q3k@q3k.org";
      password = "{SSHA}BEccJdtnhVLDzOn+pxNfayNi3QFcEABE";
    }
    {
      username = "qyliss";
      displayName = "Alyssa Ross";
      email = "hi@alyssa.is";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$+uTpAKrN452D8wa7OFqPnw$GYi9/zns5iJCXDp1VuTPPsa35M5vkD6+rC8riT8cEHI";
    }
    {
      username = "riking";
      displayName = "kanepyork";
      email = "rikingcoding@gmail.com";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$o2OcfhfKOry+UrcmODyQCw$qloaQgoIRDESwaA3yqPxxy8sgLk3mrjYFBbF41elVrM";
    }
    {
      username = "tazjin";
      email = "mail@tazj.in";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$wOPEl9D3kSke//oLtbvqrg$j0npwwXgaXQ/emefKUwL59tH8hdmtzbgH2rQzWSmE2Y";
    }
    {
      username = "implr";
      email = "implr@hackerspace.pl";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$SHRFps5sVgyUXYdmqGPw9g$tEx9DwKK1RjWlw52GLwOZ/iHep+QJboaZE83f1pXSwQ";
    }
    {
      username = "v";
      displayName = "V";
      email = "v@anomalous.eu";
      password = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$Wa11vk3gQKhJr1uzvtRTRQ$RHfvcC2j6rDUgWfezm05N03LeGIEezeKtmFmt+rfvM4";
    }
    {
      username = "ben";
      email = "tvl@benjojo.co.uk";
      password = "{SSHA}Zi48mSPsRMEPhff44w4RHi0SjjyhjWk1";
    }
  ];
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
    suffix = "dc=tvl,dc=fyi";
    rootdn = "cn=admin,dc=tvl,dc=fyi";
    rootpw = "{ARGON2}$argon2id$v=19$m=65536,t=2,p=1$OfcgkOQ96VQ3aJj7NfA9vQ$oS6HQOkYl/bUYg4SejpltQYy7kvqx/RUxvoR4zo1vXU";

    # ACL configuration
    extraDatabaseConfig = ''
      # Allow users to change their own password
      access to attrs=userPassword
        by self write
        by anonymous auth
        by users none

      # Allow default read access to other directory elements
      access to * by * read
    '';

    extraConfig = ''
      moduleload pw-argon2
    '';

    # Contents are immutable at runtime, and adding user accounts etc.
    # is done statically in the LDIF-formatted contents in this folder.
    declarativeContents = ''
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
