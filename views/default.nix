# Export configuration for the views.
{ depot, pkgs, ... }:

let
  export-tvix = depot.tools.releases.filteredGitPush {
    filter = ":workspace=views/tvix";
    remote = "git@github.com:tvlfyi/tvix.git";
    ref = "refs/heads/canon";
  };

  export-kit = depot.tools.releases.filteredGitPush {
    filter = ":unsign:workspace=views/kit";
    remote = "git@github.com:tvlfyi/kit.git";
    ref = "refs/heads/canon";
  };
in
(pkgs.runCommandLocal "export-views" { } ''
  echo "no-op carrier target for repo export steps" | tee $out
'').overrideAttrs
  (_: {
    meta.ci.extraSteps = {
      inherit export-tvix export-kit;
    };
  })
