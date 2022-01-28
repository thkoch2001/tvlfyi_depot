{ pkgs
, depot
, ...
}:
let
  inherit ( builtins ) path toJSON;
  inherit ( depot.users.wpcarro.emacs ) initEl runScript;
  pipeline.steps = [
    {
      key = "lint-secrets";
      command = "${ pkgs.git-secrets }/bin/git-secrets --scan-history";
      label = ":broom: lint secrets";
    }
    {
      key = "init-emacs";
      command =
        let
          scriptEl = path { path = ./script.el; name = "script.el"; };
          runScriptEl = runScript { script = scriptEl; };
        in
        "${ runScriptEl } ${ initEl }";
      label = ":gnu: initialize Emacs";
      depends_on = "lint-secrets";
    }
  ];
in
pkgs.writeText "pipeline.yaml" ( toJSON pipeline )
