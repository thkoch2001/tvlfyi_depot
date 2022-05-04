{ depot, pkgs, ... }:

let
  inherit (pkgs.emacsNativeComp.pkgs) withPackages;

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
    epkgs.melpaPackages.direnv
    epkgs.melpaPackages.dockerfile-mode
    epkgs.melpaPackages.haskell-mode
    epkgs.melpaPackages.jq-mode
    epkgs.melpaPackages.languagetool
    epkgs.melpaPackages.lsp-mode
    epkgs.melpaPackages.lsp-haskell
    epkgs.melpaPackages.markdown-mode
    epkgs.melpaPackages.meson-mode
    epkgs.melpaPackages.nix-mode
    epkgs.melpaPackages.sly
    epkgs.melpaPackages.yaml-mode
    epkgs.rust-mode
    epkgs.urweb-mode
    # misc
    epkgs.melpaPackages.hl-todo
    epkgs.melpaPackages.paredit
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
          ;; bqn-mode
          (setq bqn-interpreter-path "${pkgs.cbqn}/bin/BQN")

          ;; languagetool
          (setq languagetool-java-bin "${pkgs.jre}/bin/java"
                languagetool-console-command "${pkgs.languagetool}/share/languagetool-commandline.jar"
                languagetool-server-command "${pkgs.languagetool}/share/languagetool-server.jar"
                languagetool-java-arguments '("-Dfile.encoding=UTF-8"))

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
(pkgs.writeShellScriptBin "emacs" ''
  exec ${emacs}/bin/emacs          \
    --no-init-file                 \
    --directory ${configDirectory} \
    --eval "(require 'init)"       \
    "$@"
'').overrideAttrs (super: {
  buildCommand = ''
    ${super.buildCommand}

    ln -s "${emacs}/bin/emacsclient" "$out/bin/emacsclient"
  '';
})
