# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.
{ depot, lib, pkgs, ... }:

pkgs.makeOverridable
  ({ emacs ? pkgs.emacs29 }:
  let
    emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages;

    # If switching telega versions, use this variable because it will
    # keep the version check, binary path and so on in sync.
    currentTelega = epkgs: epkgs.melpaPackages.telega;

    # $PATH for binaries that need to be available to Emacs
    emacsBinPath = lib.makeBinPath [
      (currentTelega pkgs.emacsPackages)
      pkgs.libwebp # for dwebp, required by telega
    ];

    identity = x: x;

    # tree-sitter grammars for various ts-modes
    customTreesitGrammars = emacs.pkgs.treesit-grammars.with-grammars (g: with g; [
      tree-sitter-bash
      tree-sitter-c
      tree-sitter-cmake
      tree-sitter-cpp
      tree-sitter-dockerfile
      tree-sitter-go
      tree-sitter-java
      tree-sitter-json
      tree-sitter-latex
      tree-sitter-nix
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-toml
      tree-sitter-yaml
    ]);

    tazjinsEmacs = pkgfun: (emacsWithPackages (epkgs: pkgfun (with epkgs; [
      ace-link
      ace-window
      avy
      bazel
      browse-kill-ring
      cargo
      clojure-mode
      company
      consult
      deft
      direnv
      elixir-mode
      elm-mode
      erlang
      exwm
      go-mode
      google-c-style
      gruber-darker-theme
      haskell-mode
      ht
      hydra
      idle-highlight-mode
      inspector
      jq-mode
      kotlin-mode
      kubernetes
      magit
      markdown-toc
      multiple-cursors
      nginx-mode
      nix-mode
      notmuch
      paredit
      password-store
      pinentry
      prescient
      protobuf-mode
      rainbow-delimiters
      rainbow-mode
      request
      restclient
      rust-mode
      sly
      string-edit-at-point
      telephone-line
      terraform-mode
      undo-tree
      uuidgen
      vertico
      vterm
      web-mode
      websocket
      which-key
      xelb
      yasnippet
      zetteldeft
      zoxide

      # Wonky stuff
      (currentTelega epkgs)
      customTreesitGrammars # TODO(tazjin): how is this *supposed* to work?!

      # Custom depot packages (either ours, or overridden ones)
      tvlPackages.dottime
      tvlPackages.nix-util
      tvlPackages.passively
      tvlPackages.rcirc
      tvlPackages.term-switcher
      tvlPackages.tvl

      # Dynamic/native modules
      depot.users.tazjin.gio-list-apps
    ])));

    # Tired of telega.el runtime breakages through tdlib
    # incompatibility. Target to make that a build failure instead.
    tdlibCheck =
      let
        tgEmacs = emacsWithPackages (epkgs: [ (currentTelega epkgs) ]);
        verifyTdlibVersion = builtins.toFile "verify-tdlib-version.el" ''
          (require 'telega)
          (defvar tdlib-version "${pkgs.tdlib.version}")
          (when (or (version< tdlib-version
                              telega-tdlib-min-version)
                    (and telega-tdlib-max-version
                          (version< telega-tdlib-max-version
                                    tdlib-version)))
             (message "Found TDLib version %s, but require %s to %s"
                     tdlib-version telega-tdlib-min-version telega-tdlib-max-version)
            (kill-emacs 1))
        '';
      in
      pkgs.runCommand "tdlibCheck" { } ''
        export PATH="${emacsBinPath}:$PATH"
        ${tgEmacs}/bin/emacs --script ${verifyTdlibVersion} && touch $out
      '';
  in
  lib.fix
    (self: l: f: (pkgs.writeShellScriptBin "tazjins-emacs" ''
      export PATH="${emacsBinPath}:$PATH"
      exec ${tazjinsEmacs f}/bin/emacs \
        --debug-init \
        --no-site-file \
        --no-site-lisp \
        --no-init-file \
        --directory ${./config} ${if l != null then "--directory ${l}" else ""} \
        --eval "(add-to-list 'treesit-extra-load-path \"${customTreesitGrammars}/lib\")" \
        --eval "(require 'init)" $@
    '').overrideAttrs
      (_: {
        passthru = {
          # Expose original Emacs used for my configuration.
          inherit emacs;

          # Expose the pure emacs with all packages.
          emacsWithPackages = tazjinsEmacs f;

          # Call overrideEmacs with a function (pkgs -> pkgs) to modify the
          # packages that should be included in this Emacs distribution.
          overrideEmacs = f': self l f';

          # Call withLocalConfig with the path to a *folder* containing a
          # `local.el` which provides local system configuration.
          withLocalConfig = confDir: self confDir f;

          # Expose telega/tdlib version check as a target that is built in
          # CI.
          #
          # TODO(tazjin): uncomment when telega works again
          inherit tdlibCheck;
          meta.ci.targets = [ "tdlibCheck" ];
        };
      }))
    null
    identity
  )
{ }
