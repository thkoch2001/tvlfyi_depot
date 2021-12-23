{ pkgs, depot, ... }:

let
  inherit (builtins) fetchGit path toJSON;
  inherit (depot.users.wpcarro.emacs) initEl runScript;

  elispLintSrc = fetchGit {
    url = "https://github.com/gonewest818/elisp-lint";
    rev = "2b645266be8010a6a49c6d0ebf6a3ad5bd290ff4";
  };

  pipeline.steps = [
    {
      key = "lint-secrets";
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint secrets";
    }
    {
      key = "init-emacs";
      command = let
        scriptEl = path {
          path = ./script.el;
          name = "script.el";
        };
        runScriptEl = runScript {
          script = scriptEl;
        };
      in "${runScriptEl} ${initEl}";
      label = ":gnu: initialize Emacs";
      depends_on = "lint-secrets";
    }
  ];
in pkgs.writeText "pipeline.yaml" (toJSON pipeline)
