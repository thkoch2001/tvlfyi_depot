{ lib ? import <nixpkgs/lib> }:

let
  chomp = lib.removeSuffix "\n";
  lines = s: builtins.filter builtins.isString (builtins.split "\n" (chomp s));
  sum = builtins.foldl' builtins.add 0;

  day01 =
    let
      input =
        builtins.map
          (elf:
            sum (builtins.map builtins.fromJSON (lines elf))
          )
          (
            builtins.filter builtins.isString (
              builtins.split "\n\n" (builtins.readFile ./01/input)
            )
          );
    in
    {
      "1" = builtins.foldl' lib.max (-1) input;
      "2" = sum (lib.sublist 0 3 (lib.sort (a: b: a >= b) input));
    };

in

{
  inherit day01;
}
