# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.
#
# On NixOS machines, this Emacs currently does not support
# Imagemagick, see https://github.com/NixOS/nixpkgs/issues/70631.
#
# Forcing Emacs to link against Imagemagick currently causes libvterm
# to segfault, which is a lot less desirable than not having telega
# render images correctly.
{ lib, pkgs, ... }:

pkgs.makeOverridable({ emacs ? pkgs.emacsGcc }:
let
  emacsWithPackages = (pkgs.emacsPackagesGen emacs).emacsWithPackages;

  # $PATH for binaries that need to be available to Emacs
  emacsBinPath = lib.makeBinPath [ pkgs.emacsPackages.telega ];

  identity = x: x;

  tazjinsEmacs = pkgfun: (emacsWithPackages(epkgs: pkgfun(with epkgs; [
    ace-link
    ace-window
    avy
    bazel
    browse-kill-ring
    cargo
    clojure-mode
    cmake-mode
    company
    counsel
    counsel-notmuch
    d-mode
    direnv
    dockerfile-mode
    eglot
    elfeed
    elixir-mode
    elm-mode
    erlang
    exwm
    flymake
    go-mode
    google-c-style
    gruber-darker-theme
    haskell-mode
    ht
    hydra
    idle-highlight-mode
    ivy
    ivy-prescient
    jq-mode
    kotlin-mode
    lsp-mode
    magit
    markdown-toc
    meson-mode
    multi-term
    multiple-cursors
    nginx-mode
    nix-mode
    notmuch
    paredit
    password-store
    pinentry
    polymode
    prescient
    protobuf-mode
    rainbow-delimiters
    rainbow-mode
    refine
    request
    restclient
    rust-mode
    sly
    string-edit
    swiper
    telega
    telephone-line
    terraform-mode
    toml-mode
    transient
    undo-tree
    use-package
    uuidgen
    vterm
    web-mode
    websocket
    which-key
    xelb
    yaml-mode
    yasnippet

    # Custom depot packages (either ours, or overridden ones)
    tvlPackages.dottime
    tvlPackages.nix-util
    tvlPackages.passively
    tvlPackages.rcirc
    tvlPackages.term-switcher
    tvlPackages.tvl
  ])));

  # Tired of telega.el runtime breakages through tdlib
  # incompatibility. Target to make that a build failure instead.
  tdlibCheck =
    let
      tgEmacs = emacsWithPackages(epkgs: [ epkgs.telega ]);
      verifyTdlibVersion = builtins.toFile "verify-tdlib-version.el" ''
        (require 'telega)
        (when (or (version< (plist-get telega--options :version)
                            telega-tdlib-min-version)
                  (version< telega-tdlib-max-version
                            (plist-get telega--options :version)))
          (error "Found TDLib version %s, but require %s < version < %s"
                 (plist-get telega--options :version)
                 telega-tdlib-min-version telega-tdlib-max-version))
       '';
    in pkgs.runCommandNoCC "tdlibCheck" {} ''
      export PATH="${emacsBinPath}:$PATH"
      ${tgEmacs}/bin/emacs --script ${verifyTdlibVersion} && touch $out
    '';
in lib.fix(self: l: f: pkgs.writeShellScriptBin "tazjins-emacs" ''
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
    withLocalEmacs = emacsBin: pkgs.writeShellScriptBin "tazjins-emacs" ''
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

    # Expose telega/tdlib version check as a target that is built in
    # CI.
    inherit tdlibCheck;
    meta.targets = [ "tdlibCheck" ];
  }) null identity
) {}
