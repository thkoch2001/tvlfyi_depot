{ pkgs, ... }:

pkgs.fetchFromGitHub {
  owner = "RustSec";
  repo = "advisory-db";
  # TODO(Profpatsch): this will have to be updated regularly, how?
  rev = "113188c62380753f01ff0df5edb7d67a300b143a";
  sha256 = "0v086ybwr71zgs5nv8yr4w2w2d4daxx6in2s1sjb4m41q1r9p0wj";
}
