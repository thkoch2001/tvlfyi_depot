{ lib, config, ... }:
with lib;
{
  options = {
    aspen.impure.clonedRepos = mkOption {
      description = "Repositories to clone";
      default = { };
      type = with types; attrsOf (
        let
          sm = submodule {
            options = {
              url = mkOption {
                type = nullOr str;
                description = "URL of repository to clone";
                default = null;
              };

              github = mkOption {
                type = nullOr str;
                description = "Github owner/repo of repository to clone";
                default = null;
              };

              path = mkOption {
                type = str;
                description = "Path to clone to";
              };

              onClone = mkOption {
                type = str;
                description = ''
                  Shell command to run after cloning the repo for the first time.
                  Runs inside the repo itself.
                '';
                default = "";
              };

              after = mkOption {
                type = listOf str;
                description = "Activation hooks that this repository must be cloned after";
                default = [ ];
              };
            };
          };
        in
        addCheck sm (cr: (! isNull cr.url || ! isNull cr.github))
      );
    };
  };

  config = {
    home.activation =
      mapAttrs
        (_: { url
            , path
            , github
            , onClone
            , after
            , ...
            }:
          let repoURL = if isNull url then "git@github.com:${github}" else url;
          in hm.dag.entryAfter ([ "writeBoundary" ] ++ after) ''
            $DRY_RUN_CMD mkdir -p $(dirname "${path}")
            if [[ ! -d ${path} ]]; then
              if $DRY_RUN_CMD git clone "${repoURL}" "${path}"; then
                pushd ${path}
                $DRY_RUN_CMD ${onClone}
                popd
              else
                echo "Git repository ${path} failed to clone"
              fi
            fi
          '')
        config.aspen.impure.clonedRepos;
  };
}
