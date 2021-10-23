{ pkgs, ... }:

{
  install = pkgs.writeShellScript "install-configs" ''
    cd "$WPCARRO/configs" && ${pkgs.stow}/bin/stow --target="$HOME" .
  '';

  uninstall = pkgs.writeShellScript "uninstall-configs" ''
    cd "$WPCARRO/configs" && ${pkgs.stow}/bin/stow --delete --target="$HOME" .
  '';
}
