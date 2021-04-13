# This overlay is intended for applying bug fixes to nixpkgs
# temporarily until a channel bump resolves them. If you are
# adding something here and there isn't an upstream fix or
# PR for it yet, consider contributing it to nixpkgs as well.
# Try to reference the PR / upstream commit that will allow
# us to remove the fix from this file. Permanent changes
# should go to `./tvl.nix`.
#
# Ideally, this overlay is always empty.

{ ... }:
self: super: {
  # Already fixed on master (commit unknown)
  awscli2 = super.awscli2.overrideAttrs (old: {
    postPatch = ''
      substituteInPlace setup.py \
        --replace "cryptography<3.4.0,>=3.3.2" "cryptograph>=3.3.2"
    '' + old.postPatch;
  });
}
