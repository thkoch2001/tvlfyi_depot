{ briefcase, pkgs, ... }:

let
  elispLintSrc = builtins.fetchGit {
    url = "https://github.com/gonewest818/elisp-lint";
    rev = "2b645266be8010a6a49c6d0ebf6a3ad5bd290ff4";
  };

  scriptEl = builtins.path {
    path = ./script.el;
    name = "script.el";
  };

  pipeline.steps = [
    {
      key = "lint-secrets";
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint secrets";
    }
    {
      key = "build-briefcase";
      command = ''
        nix-build . -I briefcase="$(pwd)" --no-out-link --show-trace
      '';
      label = ":nix: build briefcase";
      depends_on = "lint-secrets";
    }
    {
      key = "init-emacs";
      command = ''
        ${briefcase.emacs.runScript scriptEl} ${briefcase.emacs.initEl}
      '';
      label = ":gnu: initialize Emacs";
      depends_on = "build-briefcase";
    }
    {
      key = "lint-emacs";
      command = ''
        ${briefcase.emacs.nixos}/bin/wpcarros-emacs \
          --quick \
          --batch \
          --load ${elispLintSrc}/elisp-lint.el \
          --funcall elisp-lint-files-batch \
          "$@"
      '';
      label = ":gnu: lint Emacs";
      depends_on = "init-emacs";
    }
    {
      key = "build-socrates";
      command = ''
        nix-build '<nixpkgs/nixos>' \
          -I briefcase="$(pwd)" \
          -I nixpkgs=/var/lib/buildkite-agent-socrates/nixpkgs-channels \
          -I nixos-config=nixos/socrates/default.nix \
          -A system \
          --no-out-link \
          --show-trace
      '';
      label = ":nix: build socrates";
      depends_on = "build-briefcase";
    }
  ];
in pkgs.writeText "pipeline.yaml" (builtins.toJSON pipeline)
