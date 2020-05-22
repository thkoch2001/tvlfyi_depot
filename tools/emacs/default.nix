# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.
#
# On NixOS machines, this Emacs currently does not support
# Imagemagick, see https://github.com/NixOS/nixpkgs/issues/70631.
#
# Forcing Emacs to link against Imagemagick currently causes libvterm
# to segfault, which is a lot less desirable than not having telega
# render images correctly.
{ depot, lib, ... }:

let
  inherit (depot) third_party;

  emacsWithPackages = (third_party.emacsPackagesGen third_party.emacs26).emacsWithPackages;

  # $PATH for binaries that need to be available to Emacs
  emacsBinPath = lib.makeBinPath [ third_party.telega ];

  identity = x: x;

  tazjinsEmacs = pkgfun: (emacsWithPackages(epkgs: pkgfun(
  # Actual ELPA packages (the enlightened!)
  (with epkgs.elpaPackages; [
    ace-window
    avy
    flymake
    pinentry
    rainbow-mode
    undo-tree
    xelb
  ]) ++

  # MELPA packages:
  (with epkgs.melpaPackages; [
    ace-link
    browse-kill-ring
    cargo
    clojure-mode
    cmake-mode
    counsel
    counsel-notmuch
    dash-functional
    direnv
    dockerfile-mode
    eglot
    elixir-mode
    elm-mode
    erlang
    geiser
    go-mode
    gruber-darker-theme
    haskell-mode
    ht
    hydra
    idle-highlight-mode
    intero
    ivy
    ivy-pass
    ivy-prescient
    jq-mode
    kotlin-mode
    lispy
    lsp-mode
    magit
    markdown-toc
    meson-mode
    multi-term
    multiple-cursors
    nginx-mode
    nix-mode
    notmuch # this comes from pkgs.third_party
    org-journal
    org-ql
    paredit
    password-store
    pg
    polymode
    prescient
    protobuf-mode
    racket-mode
    rainbow-delimiters
    refine
    request
    restclient
    sly
    string-edit
    swiper
    telega
    telephone-line
    terraform-mode
    toml-mode
    transient
    use-package
    uuidgen
    web-mode
    websocket
    which-key
    yaml-mode
    yasnippet
  ]) ++

  # Custom packages
  (with depot.tools.emacs-pkgs; [
    dottime
    nix-util
    term-switcher

    # patched / overridden versions of packages
    depot.third_party.emacs.exwm
    depot.third_party.emacs.rcirc
    depot.third_party.emacs.vterm
  ]))));
in lib.fix(self: l: f: third_party.writeShellScriptBin "tazjins-emacs" ''
  export PATH="${emacsBinPath}:$PATH"
  exec ${tazjinsEmacs f}/bin/emacs \
    --debug-init \
    --no-site-file \
    --no-site-lisp \
    --no-init-file \
    --directory ${./config} ${if l != null then "--directory ${l}" else ""} \
    --eval "(require 'init)" $@
  '' // {
    # Call overrideEmacs with a function (pkgs -> pkgs) to modify the
    # packages that should be included in this Emacs distribution.
    overrideEmacs = f': self l f';

    # Call withLocalConfig with the path to a *folder* containing a
    # `local.el` which provides local system configuration.
    withLocalConfig = confDir: self confDir f;

    # Build a derivation that uses the specified local Emacs (i.e.
    # built outside of Nix) instead
    withLocalEmacs = emacsBin: third_party.writeShellScriptBin "tazjins-emacs" ''
      export PATH="${emacsBinPath}:$PATH"
      export EMACSLOADPATH="${(tazjinsEmacs f).deps}/share/emacs/site-lisp:"
      exec ${emacsBin} \
        --debug-init \
        --no-site-file \
        --no-site-lisp \
        --no-init-file \
        --directory ${./config} \
        ${if l != null then "--directory ${l}" else ""} \
        --eval "(require 'init)" $@
    '';
  }) null identity
