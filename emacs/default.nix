{ pkgs, ... }:

let
  # NOTE: I'm trying to keep the list of dependencies herein constrained to a
  # list of generic dependencies (i.e. not project or language specific). For
  # language-specific tooling, I'm intending to use shell.nix alongside lorri
  # and direnv.
  emacsBinPath = pkgs.lib.strings.makeBinPath (with pkgs; [
    tdesktop # native telegram client
    ripgrep
    gitAndTools.hub
    kubectl
    google-cloud-sdk
    xsv
    scrot
    clipmenu
  ]);

  emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs26).emacsWithPackages;

  wpcarrosEmacs = emacsWithPackages (epkgs:
    (with epkgs.elpaPackages; [
      exwm
    ]) ++

    (with epkgs.melpaPackages; [
      org-bullets
      sly
      notmuch
      elm-mode
      ts
      vterm
      base16-theme
      ivy-pass
      clipmon # TODO: Prefer an Emacs client for clipmenud.
      protobuf-mode # TODO: Determine if this is coming from google-emacs.
      # docker
      evil
      evil-collection
      evil-magit
      evil-commentary
      evil-surround
      key-chord
      add-node-modules-path # TODO: Assess whether or not I need this with Nix.
      web-mode
      rjsx-mode
      tide
      prettier-js
      flycheck
      diminish
      doom-themes
      telephone-line
      neotree # TODO: Remove this dependency from my config.
      which-key
      ivy
      restclient
      package-lint
      parsec
      magit-popup
      direnv
      emr
      ivy-prescient
      all-the-icons
      all-the-icons-ivy
      alert
      nix-mode
      racer
      rust-mode
      rainbow-delimiters
      racket-mode
      lispyville
      elisp-slime-nav
      py-yapf
      reason-mode
      elixir-mode
      go-mode
      company
      markdown-mode
      refine
      deferred
      magit
      request
      pcre2el
      helpful
      exec-path-from-shell # TODO: Determine if Nix solves this problem.
      yasnippet
      projectile
      deadgrep
      counsel
      counsel-projectile
      engine-mode # TODO: Learn what this is.
      eglot
      dap-mode
      lsp-ui
      company-lsp
      suggest
      paradox
      # emr
      flymake-shellcheck
      fish-mode
      tuareg
      haskell-mode
      lsp-haskell
      use-package
      general
      clojure-mode
      cider
      f
      dash
      company
      counsel
      flycheck
      ivy
      magit
    ]));

  withEmacsPath = emacsBin: pkgs.writeShellScriptBin "wpcarros-emacs" ''
    # TODO: Is this the best way to handle environment variables using Nix?
    export BRIEFCASE=$HOME/briefcase
    export DEPOT=$HOME/depot

    export PATH="${emacsBinPath}:$PATH"
    export EMACSLOADPATH="${wpcarrosEmacs.deps}/share/emacs/site-lisp:"
    exec ${emacsBin} \
      --debug-init \
      --no-site-file \
      --no-site-lisp \
      --directory ${ ./.emacs.d/vendor } \
      --directory ${ ./.emacs.d/wpc } \
     --load ${ ./.emacs.d/wpc/wpc-package.el } \
     --load ${ ./.emacs.d/init.el } \
      --no-init-file $@
  '';
in {
  # Use `nix-env -f '<briefcase>' emacs.glinux` to install `wpcarro-emacs` on
  # gLinux machines. This will ensure that X and GL linkage behaves as expected.
  glinux = withEmacsPath "/usr/bin/emacs";

  # Use `nix-env -f '<briefcase>' emacs.nixos` to install `wpcarros-emacs` on
  # NixOS machines.
  nixos = withEmacsPath "${wpcarrosEmacs}/bin/emacs";
}
