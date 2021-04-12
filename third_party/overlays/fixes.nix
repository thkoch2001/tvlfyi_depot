# This overlay is intended for applying bug fixes to nixpkgs
# temporarily until a channel bump resolves them. If you are
# adding something here and there isn't an upstream fix or
# PR for it yet, consider contributing it to nixpkgs as well.
# Try to reference the PR / upstream commit that will allow
# us to remove the fix from this file.

{ ... }:

self: super: {
  # Fix evaluation of docker when cross-compiling
  # https://github.com/NixOS/nixpkgs/pull/119242
  docker = super.docker.overrideAttrs (old: {
    buildInputs = super.lib.filter
      (drv: drv.name != "hook") # the descriptive name of makeWrapper
      old.buildInputs;
    nativeBuildInputs = old.nativeBuildInputs ++ [
      self.buildPackages.makeWrapper
    ];
  });
}
