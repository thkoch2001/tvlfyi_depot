{ config, pkgs, ... }:

{
  imports = [
    ./modules/alacritty.nix
    ./modules/emacs.nix
    ./modules/i3.nix
    ./modules/shell.nix
    ./modules/vim.nix
    ./modules/alsi.nix
    ./modules/lib/cloneRepo.nix

  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  xsession.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "19.09";

  home.packages = with pkgs; [
    nix-prefetch-github
    htop
    powertop
    pass
    gitAndTools.hub
    shellcheck
    gnupg

    # Spotify...etc
    spotify
    playerctl

    # games
    crawl
    dwarf-fortress
  ];

  nixpkgs.config.allowUnfree = true;

  programs.git = {
    enable = true;
    userEmail = "root@gws.fyi";
    userName  = "Griffin Smith";
  };

  services.redshift = {
    enable = true;
    provider = "geoclue2";
  };

  services.pasystray.enable = true;

  impure.clonedRepos.passwordStore = {
    github = "glittershark/pass";
    path = ".password-store";
  };

  services.gpg-agent = {
    enable = true;
  };
}
