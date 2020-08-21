{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = let
        # Regexes to detect sensitive information
        patterns = pkgs.writeText "secrets.txt" ''
          (A3T[A-Z0-9]|AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16}
          ("|')?(AWS|aws|Aws)?_?(SECRET|secret|Secret)?_?(ACCESS|access|Access)?_?(KEY|key|Key)("|')?\s*(:|=>|=)\s*("|')?[A-Za-z0-9/\+=]{40}("|')?
          ("|')?(AWS|aws|Aws)?_?(ACCOUNT|account|Account)_?(ID|id|Id)?("|')?\s*(:|=>|=)\s*("|')?[0-9]{4}\-?[0-9]{4}\-?[0-9]{4}("|')?
          AIza[0-9A-Za-z_-]{35}
          [0-9]+-[0-9A-Za-z_]{32}\.apps\.googleusercontent\.com
          (^|[^0-9A-Za-z/+])1/[0-9A-Za-z_-]{43}
          (^|[^0-9A-Za-z/+])1/[0-9A-Za-z_-]{64}
          ya29\.[0-9A-Za-z_-]+
          (sk|pk)_(test|live)_[a-zA-Z0-9]{99}
        '';
      in ''
        cat .git/config
        ${pkgs.git-secrets}/bin/git-secrets --add-provider -- cat ${patterns}
        ${pkgs.git-secrets}/bin/git-secrets --scan-history
      '';
      label = ":broom: lint";
    }
    {
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":nix: build";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
