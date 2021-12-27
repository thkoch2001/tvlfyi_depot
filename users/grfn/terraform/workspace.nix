{ pkgs, depot, ... }:
name: { plugins }: module_tf:

let

  inherit (pkgs) lib runCommandNoCC writeText writeScript;
  inherit (lib) filterAttrsRecursive;

  allPlugins = (p: plugins p ++ (with p; [
    external
    local
    tls
    p.null
  ]));

  tf = pkgs.terraform.withPlugins allPlugins;

  cleanTerraform = filterAttrsRecursive (k: _: ! (builtins.elem k [
    "__readTree"
    "__readTreeChildren"
  ]));

  plugins_tf = {
    terraform.required_providers = (builtins.listToAttrs (map (p: {
      name = lib.last (lib.splitString "/" p.provider-source-address);
      value = {
        source = p.provider-source-address;
        version = p.version;
      };
    }) (allPlugins pkgs.terraform.plugins)));
  };


  module_tf' = module_tf // {
    inherit (depot.users.grfn.terraform) globals;
    plugins = plugins_tf;
  };

  module = runCommandNoCC "module" {} ''
    mkdir $out
    ${lib.concatStrings (lib.mapAttrsToList (k: config_tf:
      (let
        # TODO: filterAttrsRecursive?
        configJson = writeText "${k}.tf.json"
          (builtins.toJSON (cleanTerraform config_tf));
      in ''
        ${pkgs.jq}/bin/jq . ${configJson} > $out/${lib.escapeShellArg k}.tf.json
      ''))
      (cleanTerraform module_tf'))}
  '';


  tfcmd = writeScript "${name}-tfcmd" ''
    set -e
    dir="''${TF_STATE_ROOT:-$HOME/tfstate}/${name}"
    cd "$dir"
    rm -f *.json
    cp ${module}/*.json .
    exec ${tf}/bin/terraform "$(basename "$0")"
  '';

  init = writeScript "${name}-init" ''
    set -e
    dir="''${TF_STATE_ROOT:-$HOME/tfstate}/${name}"
    [ -d "$dir" ] || mkdir -p "$dir"
    cd "$dir"
    rm -f *.json
    cp ${module}/*.json .
    exec ${tf}/bin/terraform init
  '';

  # TODO: import (-config)
  tfcmds = runCommandNoCC "${name}-tfcmds" {} ''
    mkdir -p $out/bin
    ln -s ${init} $out/bin/init
    ln -s ${tfcmd} $out/bin/validate
    ln -s ${tfcmd} $out/bin/plan
    ln -s ${tfcmd} $out/bin/apply
    ln -s ${tfcmd} $out/bin/destroy
  '';

in {
  inherit name module;
  terraform = tf;
  cmds = tfcmds;

  # run = {
  #   init = depot.nix.nixRunWrapper "init" tfcmds;
  #   validate = depot.nix.nixRunWrapper "validate" tfcmds;
  #   plan = depot.nix.nixRunWrapper "plan" tfcmds;
  #   apply = depot.nix.nixRunWrapper "apply" tfcmds;
  #   destroy = depot.nix.nixRunWrapper "destroy" tfcmds;
  # };

  test = runCommandNoCC "${name}-test" {} ''
    set -e
    export TF_STATE_ROOT=$(pwd)
    ${tfcmds}/bin/init
    ${tfcmds}/bin/validate
    touch $out
  '';

  meta.targets = [ "module" "test" ];
}
