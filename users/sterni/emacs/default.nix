{ depot, pkgs, ... }:

let
  # emacsPgtkNativeComp is defined in emacs-overlay
  emacs = (pkgs.emacsPackagesFor pkgs.emacsPgtkNativeComp).withPackages (epkgs: [
    (epkgs.bqn-mode.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        # emacs HEAD doesn't like a missing require in bqn-mode
        (pkgs.fetchpatch {
          name = "bqn-mode-emacs-head.patch";
          url = "https://github.com/museoa/bqn-mode/pull/9/commits/b62d7aff12201a079f60c1842d86610b9331bf53.patch";
          sha256 = "1i5f2w7rcd9vx8x50ydwqnkxd5c824p5kxj2c00kq3lmiczhr41a";
        })
      ];
    }))
    #epkgs.elpaPackages.ada-mode
    epkgs.elpaPackages.rainbow-mode
    epkgs.elpaPackages.undo-tree
    epkgs.elpaPackages.which-key
    epkgs.melpaPackages.adoc-mode
    epkgs.melpaPackages.cmake-mode
    epkgs.melpaPackages.direnv
    epkgs.melpaPackages.dockerfile-mode
    epkgs.melpaPackages.editorconfig
    epkgs.melpaPackages.elfeed
    epkgs.melpaPackages.evil
    epkgs.melpaPackages.evil-collection
    epkgs.melpaPackages.haskell-mode
    epkgs.melpaPackages.hl-todo
    epkgs.melpaPackages.jq-mode
    epkgs.melpaPackages.languagetool
    epkgs.melpaPackages.lsp-haskell
    epkgs.melpaPackages.lsp-mode
    epkgs.melpaPackages.lsp-ui
    epkgs.melpaPackages.magit
    epkgs.melpaPackages.markdown-mode
    epkgs.melpaPackages.meson-mode
    epkgs.melpaPackages.nix-mode
    epkgs.melpaPackages.org-clock-csv
    epkgs.melpaPackages.paredit
    epkgs.melpaPackages.rainbow-delimiters
    epkgs.melpaPackages.sly
    epkgs.melpaPackages.use-package
    epkgs.melpaPackages.yaml-mode
    epkgs.rust-mode
    epkgs.tvlPackages.tvl
    epkgs.urweb-mode

    # TODO(sterni): until org-tracker is part of depot
    epkgs.ivy
    epkgs.dash
    epkgs.s
    epkgs.jiralib2
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
                languagetool-server-command "${pkgs.languagetool}/share/languagetool-server.jar")

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
