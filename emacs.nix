{ pkgs ? import <nixpkgs> {}
, tazjinsPkgs ? import (builtins.fetchGit "https://git.tazj.in") {
    rev = "4c0e0d715f21eeb62594d198ba1eeccb1a2cfb13";
  }
}:

let
  # TODO: Move this function definition to a prelude.nix or elsewhere.
  identity = x: x;

  # Here is a whitelist of all of the binary dependencies that Emacs relies
  # on. These are separate from Emacs libraries like dash.el.
  emacsBinPath = pkgs.lib.strings.makeBinPath [ pkgs.terminator ];

  emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs26).emacsWithPackages;

  # TODO: Learn more about melpa versus elpa to have a preference.
  wpcarrosEmacs = emacsWithPackages (epkgs:
    (with epkgs.elpaPackages; [
      exwm
    ]) ++

    (with epkgs.melpaPackages; [
      base16-theme
      ivy-pass
      clipmon # TODO: Prefer an Emacs client for clipmenud.
      protobuf-mode # TODO: Determine if this is coming from google-emacs.
      docker
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
      neotree # TODO: Remove this dependency from my config.
      which-key
      ivy
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
      emr
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
    ]) ++

    # tazjin's packages
    (with tazjinsPkgs.tools.emacs-pkgs; [
      dottime
      term-switcher
    ]));

# TODO: Do I need `pkgs.lib.fix`?
in pkgs.lib.fix(self: l: f: pkgs.writeShellScriptBin "wpcarros-emacs" ''
   export PATH="${emacsBinPath}:$PATH"
   exec ${wpcarrosEmacs}/bin/emacs \
     --debug-init \
     --no-site-file \
     --no-site-lisp \
     --directory ${ ./configs/shared/emacs.d/vendor } \
     --directory ${ ./configs/shared/emacs.d/wpc } \
     --directory ${ ./configs/shared/emacs.d/wpc/packages } \
     --load ${ ./configs/shared/emacs.d/init.el } \
     --no-init-file $@
'' // {
  # TODO: Ascertain whether I need this.
  overrideEmacs = f': self l f';

  # Call with a local.el file containing local system configuration.
  withLocalConfig = confDir: self confDir f;

  # This accepts the path to a non-Nix built Emacs, so that X and GL linkage
  # behaves as expected.
  withLocalEmacs = emacsBin: pkgs.writeShellScriptBin "wpcarros-emacs" ''
    export PATH="${emacsBinPath}:$PATH"
    export EMACSLOADPATH="${wpcarrosEmacs.deps}/share/emacs/site-lisp:"
    exec ${emacsBin} \
      --debug-init \
      --no-site-file \
      --no-site-lisp \
      --directory ${ ./configs/shared/emacs.d/vendor } \
      --directory ${ ./configs/shared/emacs.d/wpc } \
      --directory ${ ./configs/shared/emacs.d/wpc/packages } \
      --load ${ ./configs/shared/emacs.d/init.el } \
      --no-init-file $@
  '';
}) null identity
