# Emacs overlay from https://github.com/nix-community/emacs-overlay
{ ...
}:
let
  # from 2022-01-04
  commit = "a463c3bcbb04b4b744a259587081786ded8fd5b5";
  src =
    builtins.fetchTarball
      {
        url = "https://github.com/nix-community/emacs-overlay/archive/${ commit }.tar.gz";
        sha256 = "1b7rmshf1wc9wcml7jlzggdzilj644brk5m49fry6lv53vqmykjq";
      };
in
import src
