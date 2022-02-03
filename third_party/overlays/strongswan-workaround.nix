# Workaround for an issue where strongswan 5.9.5 can not connect to
# some servers that do not have a mitigation for CVE-2021-45079
# applied.
#
# Of course ideally the servers would be patched, but the world is not
# ideal.
#
# Only intended for use by //users/tazjin/nixos/...
{ ... }:

self: super: {
  # Downgrade strongswan to 5.9.4
  #
  # See https://github.com/NixOS/nixpkgs/pull/156567
  strongswan = super.strongswan.overrideAttrs (_: rec {
    version = "5.9.4";

    src = self.fetchFromGitHub {
      owner = "strongswan";
      repo = "strongswan";
      rev = version;
      sha256 = "1y1gs232x7hsbccjga9nbkf4bbi5wxazlkg00qd2v1nz86sfy4cd";
    };
  });
}
