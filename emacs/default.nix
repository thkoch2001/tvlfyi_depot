{
  pkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  ...
}:

let
  utils = import ~/briefcase/utils;
  emacsBinPath = pkgs.lib.strings.makeBinPath (with pkgs; [
    scrot
    clipmenu
    ocaml
    ocamlformat
  ]);
  emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs26).emacsWithPackages;
  wpcarrosEmacs = emacsWithPackages (epkgs:
    (with epkgs.elpaPackages; [
      exwm
    ]) ++

    (with epkgs.melpaPackages; [
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
    ]) ++

    (with depot.tools.emacs-pkgs; [
      dottime
      term-switcher
    ]));

# TODO: Do I need `pkgs.lib.fix`?
in pkgs.lib.fix(self: l: f: pkgs.writeShellScriptBin "wpcarros-emacs" ''
   # TODO: Is this the best way to handle environment variables using Nix?
   export BRIEFCASE=$HOME/briefcase
   export DEPOT=$HOME/depot

   export PATH="${emacsBinPath}:$PATH"
   exec ${wpcarrosEmacs}/bin/emacs \
     --debug-init \
     --no-site-file \
     --no-site-lisp \
     --directory ${ ./.emacs.d/vendor } \
     --directory ${ ./.emacs.d/wpc } \
     --load ${ ./.emacs.d/wpc/wpc-package.el } \
     --load ${ ./.emacs.d/init.el } \
     --no-init-file $@
'' // {
  # TODO: Ascertain whether I need this.
  overrideEmacs = f': self l f';

  # Call with a local.el file containing local system configuration.
  withLocalConfig = confDir: self confDir f;

  # This accepts the path to an Emacs binary outside of /nix/store. On gLinux,
  # this will ensure that X and GL linkage behaves as expected.
  withLocalEmacs = emacsBin: pkgs.writeShellScriptBin "wpcarros-emacs" ''
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
}) null utils.identity
