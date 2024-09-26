args: {
  dunstrc = ./dunstrc;
  niri = ./niri.config.kdl;
  waybar = {
    config = import ./waybar/config.nix args;
    style = ./waybar/style.css;
  };
}
