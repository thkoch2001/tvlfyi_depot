{ depot, ... }:

let
  inherit (depot.users.sterni.nix)
    num
    ;
in

rec {
  # In C++ Nix, the required builtins have been added in version 2.4
  ceil = builtins.ceil or (throw "Nix implementation is missing builtins.ceil");
  floor = builtins.floor or (throw "Nix implementation is builtins.floor");

  truncate = f: if f >= 0 then floor f else ceil f;
  round = f:
    let
      s = num.sign f;
      a = s * f;
    in
    s * (if a >= floor a + 0.5 then ceil a else floor a);

  intToFloat = i: i * 1.0;
}
