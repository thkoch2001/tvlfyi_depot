{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitLab {
    domain = "gitlab.common-lisp.net";
    owner = "cl-smtp";
    repo = "cl-smtp";
    rev = "ed47d326fad867ee11323fa3a0f307b5d40e8f2b";
    sha256 = "0vjjfapcrdc5671jz2d24h8zvpz7skq1x6pi9fvw6ls5sgms6fr0";
  };

in depot.nix.buildLisp.library {
  name = "cl-smtp";
  deps = with depot.third_party.lisp; [
    usocket
    trivial-gray-streams
    flexi-streams
    cl-base64
    cl-plus-ssl
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "attachments.lisp"
    "cl-smtp.lisp"
    "mime-types.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
