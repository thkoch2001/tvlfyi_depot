# Utility for invoking slappasswd with the correct options for
# creating an ARGON2 password hash.
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "hash-password" ''
  ${depot.third_party.openldap}/bin/slappasswd -o module-load=pw-argon2 -h '{ARGON2}'
''
