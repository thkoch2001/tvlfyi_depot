{ depot, pkgs, ... }:

let
  inherit (pkgs.emacsGcc.pkgs) withPackages;

  emacs = withPackages (epkgs: [
    # basic setup
    epkgs.elpaPackages.undo-tree
    epkgs.melpaPackages.evil
    epkgs.melpaPackages.evil-collection
    epkgs.melpaPackages.use-package
    # languages
    epkgs.bqn-mode
    epkgs.elpaPackages.ada-mode
    epkgs.melpaPackages.adoc-mode
    epkgs.melpaPackages.dockerfile-mode
    epkgs.melpaPackages.haskell-mode
    epkgs.melpaPackages.jq-mode
    epkgs.melpaPackages.markdown-mode
    epkgs.melpaPackages.nix-mode
    epkgs.melpaPackages.sly
    epkgs.melpaPackages.yaml-mode
    epkgs.rust-mode
    epkgs.urweb-mode
    # misc
    epkgs.melpaPackages.hl-todo
    epkgs.elpaPackages.rainbow-mode
    epkgs.melpaPackages.rainbow-delimiters
    # beyond text editing
    epkgs.melpaPackages.elfeed
    epkgs.melpaPackages.magit
    epkgs.tvlPackages.tvl
  ]);

  configDirectory = pkgs.symlinkJoin {
    name = "emacs.d";
    paths = [
      ./.
      (pkgs.writeTextFile {
        name = "injected-emacs.d";
        destination = "/nix-inject.el";
        text = ''
          (setq bqn-interpreter-path "${pkgs.cbqn}/bin/BQN")

          (provide 'nix-inject)
        '';
      })
    ];
    postBuild = ''
      rm "$out/default.nix"
    '';
  };
in

# sadly we can't give an init-file via the command line
pkgs.writeShellScriptBin "emacs" ''
  exec ${emacs}/bin/emacs          \
    --no-init-file                 \
    --directory ${configDirectory} \
    --eval "(require 'init)"       \
    "$@"
''
