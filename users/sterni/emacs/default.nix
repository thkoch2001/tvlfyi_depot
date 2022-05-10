{ depot, pkgs, ... }:

let
  inherit (pkgs.emacsNativeComp.pkgs) withPackages;

  emacs = withPackages (epkgs: [
    epkgs.bqn-mode
    epkgs.elpaPackages.ada-mode
    epkgs.elpaPackages.rainbow-mode
    epkgs.elpaPackages.undo-tree
    epkgs.melpaPackages.adoc-mode
    epkgs.melpaPackages.cmake-mode
    epkgs.melpaPackages.direnv
    epkgs.melpaPackages.dockerfile-mode
    epkgs.melpaPackages.elfeed
    epkgs.melpaPackages.evil
    epkgs.melpaPackages.evil-collection
    epkgs.melpaPackages.haskell-mode
    epkgs.melpaPackages.hl-todo
    epkgs.melpaPackages.jq-mode
    epkgs.melpaPackages.languagetool
    epkgs.melpaPackages.lsp-haskell
    epkgs.melpaPackages.lsp-mode
    epkgs.melpaPackages.magit
    epkgs.melpaPackages.markdown-mode
    epkgs.melpaPackages.meson-mode
    epkgs.melpaPackages.nix-mode
    epkgs.melpaPackages.paredit
    epkgs.melpaPackages.rainbow-delimiters
    epkgs.melpaPackages.sly
    epkgs.melpaPackages.use-package
    epkgs.melpaPackages.yaml-mode
    epkgs.rust-mode
    epkgs.tvlPackages.tvl
    epkgs.urweb-mode
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

          ;; use bash instead of fish from SHELL for some things, as it plays
          ;; nicer with TERM=dumb, as I don't need/want vterm anyways.
          ;; We want it to source /etc/profile for some extra setup that
          ;; kicks in if TERM=dumb, meaning we can't use dash/sh mode.
          (setq shell-file-name "${pkgs.bash}/bin/bash"
                explicit-bash-args '("-l"))

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
