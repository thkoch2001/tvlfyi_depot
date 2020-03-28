{ pkgs, lib, ... }:

let
 # doom-emacs = pkgs.callPackage (builtins.fetchTarball {
 #   url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
 # }) {
 #   doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
 #                               # and packages.el files
 # };
in {
  imports = [ ./lib/cloneRepo.nix ];

  # home.packages = [ doom-emacs ];
  # home.file.".emacs.d/init.el".text = ''
  #     (load "default.el")
  # '';
  #

  home.packages = [
    # haskellPackages.Agda BROKEN
  ];

  programs.emacs.enable = true;
  home.file.".doom.d".source = ./doom.d;

  impure.clonedRepos = {
    orgClubhouse = {
      github = "glittershark/org-clubhouse";
      path = "code/org-clubhouse";
    };

    doomEmacs = {
      github = "hlissner/org-clubhouse";
      path = ".emacs.d";
      after = ["orgClubhouse"];
      onClone = "bin/doom install";
    };
  };

  # Notes
  services.syncthing = {
    enable = true;
    tray = true;
  };
}
