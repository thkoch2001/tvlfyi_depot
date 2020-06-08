# Configures an OpenLDAP instance for TVL
#
# TODO(tazjin): Configure ldaps://
{ pkgs, config, ... }:

{
  services.openldap = {
    enable = true;
    dataDir = "/var/lib/openldap";
    suffix = "dc=tvl,dc=fyi";
    rootdn = "cn=admin,dc=tvl,dc=fyi";
    rootpw = "{SSHA}yEEO6Ol2W3ritdiJzPSsjOtyPGxWF2JW";

    # Contents are immutable at runtime, and adding user accounts etc.
    # is done statically in the LDIF-formatted contents in this folder.
    declarativeContents = builtins.readFile ./contents.ldif;

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
  };
}
