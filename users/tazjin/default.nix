# //users/tazjin-specific CI configuration.
{ depot, pkgs, ... }:

let
  rustfmt = pkgs.writeShellScript "rustfmt-tazjin" ''
    ${pkgs.fd}/bin/fd -e rs | \
      ${pkgs.ripgrep}/bin/rg 'users/tazjin' | \
      xargs ${pkgs.rustfmt}/bin/rustfmt --check --config-path users/tazjin
  '';
in
depot.nix.readTree.drvTargets {
  rustfmt = rustfmt.overrideAttrs (_: {
    # rustfmt not respecting config atm, disable
    meta.ci.skip = true;

    meta.ci.extraSteps.rustfmt = {
      command = rustfmt;
    };
  });

  # Use a screen lock command that resets the keyboard layout
  # before locking, to avoid locking me out when the layout is
  # in Russian.
  screenLock = pkgs.writeShellScriptBin "tazjin-screen-lock" ''
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap us
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:super
    exec ${pkgs.xsecurelock}/bin/xsecurelock
  '';
}
