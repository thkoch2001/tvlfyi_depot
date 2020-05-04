{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    cloud-sql-proxy
    postgresql
    jupyter
    python3Packages.jupyter_core
    redis
    ngrok-2
  ];

  home.file.".ipython/profile_default/ipython_config.py".text = ''
    c.InteractiveShellApp.exec_lines = ['%autoreload 2']
    c.InteractiveShellApp.extensions = ['autoreload']
    c.TerminalInteractiveShell.editing_mode = 'vi'
  '';
}
