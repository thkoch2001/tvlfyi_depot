{ pkgs, lib, config, ... }:

with lib;

let
 # doom-emacs = pkgs.callPackage (builtins.fetchTarball {
 #   url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
 # }) {
 #   doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
 #                               # and packages.el files
 # };

  depot = config.lib.depot;

in {
  imports = [
    ./lib/cloneRepo.nix
  ];

  # home.packages = [ doom-emacs ];
  # home.file.".emacs.d/init.el".text = ''
  #     (load "default.el")
  # '';
  #

  config = mkMerge [
    {
      home.packages = with pkgs; [
        # LaTeX (for org export)
        (pkgs.texlive.combine {
          inherit (pkgs.texlive)
          scheme-basic collection-fontsrecommended ulem
          fncychap titlesec tabulary varwidth framed fancyvrb float parskip
          wrapfig upquote capt-of needspace;
        })

        ispell

        ripgrep
        coreutils
        fd
        clang
        gnutls
        emacsPackages.telega
      ];

      programs.emacs = {
        enable = true;
        package = pkgs.emacsUnstable;
        extraPackages = (epkgs:
          (with epkgs; [
            tvlPackages.dottime
            tvlPackages.tvl
            vterm
            telega
          ])
        );
      };

      grfn.impure.clonedRepos = {
        orgClubhouse = {
          github = "glittershark/org-clubhouse";
          path = "code/org-clubhouse";
        };

        doomEmacs = {
          github = "hlissner/doom-emacs";
          path = ".emacs.d";
          after = ["emacs.d"];
          onClone = "bin/doom install";
        };

        "emacs.d" = {
          github = "glittershark/emacs.d";
          path = ".doom.d";
          after = ["orgClubhouse"];
        };
      };

      programs.zsh.shellAliases = {
        "ec" = "emacsclient";
      };
    }
    (mkIf pkgs.stdenv.isLinux {
      # Notes
      services.syncthing = {
        enable = true;
        tray = true;
      };
    })
  ];
}
