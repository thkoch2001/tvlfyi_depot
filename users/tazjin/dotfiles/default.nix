{ depot, pkgs, ... }@args:

rec {
  dunstrc = ./dunstrc;
  niri = ./niri.config.kdl;
  waybar = {
    config = import ./waybar/config.nix args;
    style = pkgs.runCommandNoCC "waybar-style.css"
      {
        CHICAGO95 = depot.third_party.chicago95;
      } ''
      cat ${./waybar/style.css} | ${pkgs.envsubst}/bin/envsubst > $out
    '';
  };

  # Helper derivation for iterating on waybar config.
  waybarTest = pkgs.runCommandNoCC "waybar-conf" { } ''
    mkdir -p $out
    cat ${pkgs.writeText "waybar-conf.json" (builtins.toJSON(builtins.attrValues waybar.config))} > $out/config
    cp ${waybar.style} $out/style.css
  '';
}
