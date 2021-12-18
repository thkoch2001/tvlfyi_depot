{ depot, lib, ... }:

# Convert an attrset of strings to a list of key/value netstring pairs.
# A good minimally viable json replacement if all you need is to iterate.
# You can use e.g. `forstdin -Ed '' item` in execline to split the items
# and then get the key and value via `multidefine -d '' $item { key value }`
#
# Example:
#   { foo = "bar"; x = "abc"; }
#   => "12:3:foo,3:bar,,10:1:x,3:abc,,"
#
# Example with runExecline:
#   nix.runExecline "test" {
#     stdin = nix.netstring.attrsToKeyValList {
#       foo = "bar";
#       x = "abc";
#     };
#   } [
#     "forstdin" "-Ed" "" "item"
#     "multidefine" "-d" "" "$item" [ "key" "value" ]
#     "${pkgs.coreutils}/bin/echo" "\${key} -> \${value}"
#   ]

#   will print:
#     foo -> bar
#     x -> abc
attrs:
lib.concatStrings
  (lib.mapAttrsToList
    (k: v: depot.nix.netstring.fromString
      (depot.nix.netstring.fromString k
        + depot.nix.netstring.fromString v))
    attrs)
