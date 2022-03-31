# //users/tazjin-specific CI configuration.
{ pkgs, ... }:

let
  rustfmt = pkgs.writeShellScript "rustfmt-tazjin" ''
    ${pkgs.fd}/bin/fd -e rs | \
      ${pkgs.ripgrep}/bin/rg 'users/tazjin' | \
      xargs ${pkgs.rustfmt}/bin/rustfmt --check --config-path users/tazjin
  '';

in
rustfmt.overrideAttrs (_: {
  # rustfmt not respecting config atm, disable
  meta.ci.skip = true;

  meta.ci.extraSteps.rustfmt = {
    command = rustfmt;
  };
})
