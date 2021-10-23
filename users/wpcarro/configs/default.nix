{ pkgs, briefcase, ... }:

{
  install = pkgs.writeShellScript "install-configs" ''
    cd "$BRIEFCASE/configs" && ${pkgs.stow}/bin/stow --target="$HOME" .
  '';

  uninstall = pkgs.writeShellScript "uninstall-configs" ''
    cd "$BRIEFCASE/configs" && ${pkgs.stow}/bin/stow --delete --target="$HOME" .
  '';
}
