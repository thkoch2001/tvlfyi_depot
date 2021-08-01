{ lib, pkgs, depot, ... }:
let
  toNetstring = s:
    "${toString (builtins.stringLength s)}:${s},";

  toNetstringList = xs:
    lib.concatStrings (map toNetstring xs);

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

  rust-netstring = depot.nix.writers.rustSimpleLib {
    name = "netstring";
  } ''
    pub fn to_netstring(s: &[u8]) -> Vec<u8> {
        let len = s.len();
        // length of the integer as ascii
        let i_len = ((len as f64).log10() as usize) + 1;
        let ns_len = i_len + 1 + len + 1;
        let mut res = Vec::with_capacity(ns_len);
        res.extend_from_slice(format!("{}:", len).as_bytes());
        res.extend_from_slice(s);
        res.push(b',');
        res
    }
  '';

in depot.nix.utils.drvTargets {
  inherit
    python-netstring
    rust-netstring
      ;
}
