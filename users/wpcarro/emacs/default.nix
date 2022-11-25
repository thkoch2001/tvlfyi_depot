# My Emacs distribution, which is supporting the following platforms:
# - Linux
# - Darwin
#
# USAGE:
#   $ nix-build -A users.wpcarro.emacs.osx -o /Applications/BillsEmacs.app
{ depot, pkgs, lib, ... }:

# TODO(wpcarro): See if it's possible to expose emacsclient on PATH, so that I
# don't need to depend on wpcarros-emacs and emacs in my NixOS configurations.
let
  inherit (depot.third_party.nixpkgs) emacsPackagesFor emacs28;
  inherit (depot.users) wpcarro;
  inherit (lib) mapAttrsToList;
  inherit (lib.strings) concatStringsSep makeBinPath;
  inherit (pkgs) runCommand writeShellScriptBin;

  emacsBinPath = makeBinPath (
    wpcarro.common.shell-utils ++
    # Rust dependencies
    (with pkgs; [
      cargo
      rust-analyzer
      rustc
      rustfmt
    ]) ++
    # Misc dependencies
    (with pkgs; [
      ispell
      nix
      pass
      rust-analyzer
      rustc
      rustfmt
      xorg.xset
    ] ++
    (if pkgs.stdenv.isLinux then [
      scrot
    ] else [ ]))
  );

  emacsWithPackages = (emacsPackagesFor emacs28).emacsWithPackages;

  wpcarrosEmacs = emacsWithPackages (epkgs:
    (with wpcarro.emacs.pkgs; [
      al
      bookmark
      cycle
      list
      maybe
      set
      string
      struct
      symbol
      vterm-mgt
      zle
    ]) ++

    (with epkgs.tvlPackages; [
      tvl
    ]) ++

    (with epkgs.elpaPackages; [
      exwm
    ]) ++

    (with epkgs.melpaPackages; [
      avy
      org-bullets
      sly
      notmuch
      elm-mode
      ts
      vterm
      base16-theme
      password-store
      csharp-mode
      dockerfile-mode
      evil
      evil-collection
      evil-commentary
      evil-surround
      key-chord
      # TODO(wpcarro): Assess whether or not I need this with Nix.
      add-node-modules-path
      web-mode
      rjsx-mode
      tide
      prettier-js
      flycheck
      diminish
      doom-themes
      telephone-line
      which-key
      all-the-icons
      all-the-icons-ivy
      ivy
      ivy-clipmenu
      ivy-pass
      ivy-prescient
      restclient
      package-lint
      parsec
      magit-popup
      direnv
      alert
      nix-mode
      rust-mode
      rainbow-delimiters
      racket-mode
      lispyville
      elisp-slime-nav
      py-yapf
      reason-mode
      terraform-mode
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
      yasnippet
      projectile
      deadgrep
      counsel
      counsel-projectile
      # TODO(wpcarro): Learn what this is.
      engine-mode
      eglot
      dap-mode
      lsp-ui
      suggest
      paradox
      flymake-shellcheck
      fish-mode
      tuareg
      haskell-mode
      use-package
      general
      clojure-mode
      cider
      f
      dash
      company
      counsel
      flycheck
      emojify
      yaml-mode
    ]));

  loadPath = concatStringsSep ":" [
    ./.emacs.d/wpc
    # TODO(wpcarro): Explain why the trailing ":" is needed.
    "${wpcarrosEmacs.deps}/share/emacs/site-lisp:"
  ];

  # Transform an attrset into "export k=v" statements.
  makeEnvVars = env: concatStringsSep "\n"
    (mapAttrsToList (k: v: "export ${k}=\"${v}\"") env);

  withEmacsPath = { emacsBin, env ? { }, load ? [ ] }:
    writeShellScriptBin "wpcarros-emacs" ''
      export XMODIFIERS=emacs
      export PATH="${emacsBinPath}:$PATH"
      export EMACSLOADPATH="${loadPath}"
      ${makeEnvVars env}
      exec ${emacsBin} \
        --debug-init \
        --no-init-file \
        --no-site-file \
        --no-site-lisp \
        --load ${./.emacs.d/init.el} \
        ${concatStringsSep "\n  " (map (el: "--load ${el} \\") load)}
        "$@"
    '';

  # I can't figure out how to augment LSEnvironment.PATH such that it inherits
  # the default $PATH and adds the things that I need as well, so let's
  # hardcode the desired outcome in the meantime.
  osxDefaultPath = builtins.concatStringsSep ":" [
    "/Users/bill/.nix-profile/bin"
    "/nix/var/nix/profiles/default/bin"
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
    "/opt/X11/bin"
  ];

  infoPlist = pkgs.writeText "Info.plist" (pkgs.lib.generators.toPlist {} {
    LSEnvironment = {
      PATH = "${emacsBinPath}:${osxDefaultPath}";
    };
    CFBundleExecutable = "BillsEmacs";
    CFBundleDisplayName = "BillsEmacs";
    CFBundleIconFile = "AppIcon";
    CFBundleIconName = "AppIcon";
  });

  versionPlist = pkgs.writeText "version.plist" (pkgs.lib.generators.toPlist {} {
     ProjectName = "OSXPlatformSupport";
  });
in
depot.nix.readTree.drvTargets {
  # TODO(wpcarro): Support this with base.overrideAttrs or something similar.
  nixos = { load ? [ ] }: withEmacsPath {
    inherit load;
    emacsBin = "${wpcarrosEmacs}/bin/emacs";
  };

  # To install GUI:
  # $ nix-build -A users.wpcarro.emacs.osx -o /Applications/BillsEmacs.app
  osx = pkgs.stdenv.mkDerivation {
    pname = "bills-emacs";
    version = "0.0.1";
    src = ./.;
    dontFixup = true;
    installPhase = ''
      runHook preInstall
      APP="$out"
      mkdir -p "$APP/Contents/MacOS"
      mkdir -p "$APP/Contents/Resources"
      cp ${infoPlist}      "$APP/Contents/Info.plist"
      cp ${versionPlist}   "$APP/Contents/version.plist"
      cp ${./AppIcon.icns} "$APP/Contents/Resources/AppIcon.icns"
      echo "APPL????"  > "$APP/Contents/PkgInfo"
      cat << EOF > "$APP/Contents/MacOS/BillsEmacs"
      #!${pkgs.stdenvNoCC.shell}
      export EMACSLOADPATH="${loadPath}"
      exec ${wpcarrosEmacs}/bin/emacs \
        --debug-init \
        --no-init-file \
        --no-site-file \
        --no-site-lisp \
        --load ${./.emacs.d/init.el}
      EOF
      chmod +x "$APP/Contents/MacOS/BillsEmacs"
      runHook postInstall
    '';
    meta.platforms = [ "aarch64-darwin" ];
  };

  # Script that asserts my Emacs can initialize without warnings or errors.
  check = runCommand "check-emacs" { } ''
    # Even though Buildkite defines this, I'd still like still be able to test
    # this locally without depending on my ability to remember to set CI=true.
    export CI=true
    export PATH="${emacsBinPath}:$PATH"
    export EMACSLOADPATH="${loadPath}"
    ${wpcarrosEmacs}/bin/emacs \
      --no-site-file \
      --no-site-lisp \
      --no-init-file \
      --script ${./ci.el} \
      ${./.emacs.d/init.el} && \
    touch $out
  '';
}
