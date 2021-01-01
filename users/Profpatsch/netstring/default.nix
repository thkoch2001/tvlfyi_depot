{ lib, pkgs, depot, ... }:
let
  toNetstring = s:
    "${toString (builtins.stringLength s)}:${s},";

  toNetstringKeyVal = attrs:
    lib.concatStrings
      (lib.mapAttrsToList
        (k: v: toNetstring (toNetstring k + toNetstring v))
        attrs);

  python-netstring = depot.users.Profpatsch.writers.python3Lib {
    name = "netstring";
  } ''
    def read_netstring(bytes):
        (int_length, rest) = bytes.split(sep=b':', maxsplit=1)
        val = rest[:int(int_length)]
        # has to end on a ,
        assert(rest[len(val)] == ord(','))
        return (val, rest[len(val) + 1:])

    def read_netstring_key_val(bytes):
        (keyvalnet, rest) = read_netstring(bytes)
        (key, valnet) = read_netstring(keyvalnet)
        (val, nothing) = read_netstring(valnet)
        assert(nothing == b"")
        return (key, val, rest)

    def read_netstring_key_val_list(bytes):
        rest = bytes
        res = {}
        while rest != b"":
            (key, val, r) = read_netstring_key_val(rest)
            rest = r
            res[key] = val
        return res
  '';

  tests = import ./tests.nix {
    inherit
      depot
      pkgs
      lib
      python-netstring
      toNetstring
      toNetstringKeyVal
      ;
  };

in {
  inherit
    toNetstring
    toNetstringKeyVal
    python-netstring
    tests
      ;

}
