{ ... }:
# convert any nix string into a netstring
# (prefixed by its length) according to https://en.wikipedia.org/wiki/Netstring
#
# Examples:
#   netstring.fromString "foo"
#   => "3:foo,"
#   netstring.fromString ""
#   => "0:,"
s:
"${toString (builtins.stringLength s)}:${s},"
