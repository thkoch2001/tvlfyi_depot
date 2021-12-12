{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "Shinmera";
    repo = "trivial-mimes";
    rev = "a741fc2f567a4f86b853fd4677d75e62c03e51d9";
    sha256 = "00kcm17q5plpzdj1qwg83ldhxksilgpcdkf3m9azxcdr968xs9di";
  };

  mime-types = pkgs.runCommand "mime-types.lisp" { } ''
    substitute ${src}/mime-types.lisp $out \
      --replace /etc/mime.types ${src}/mime.types \
      --replace "(asdf:system-source-directory :trivial-mimes)" '"/bogus-dir"'
      # We want to prevent an ASDF lookup at build time since this will
      # generally fail — we are not using ASDF after all.
  '';

in depot.nix.buildLisp.library {
  name = "trivial-mimes";

  deps = [{
    sbcl = depot.nix.buildLisp.bundled "uiop";
    default = depot.nix.buildLisp.bundled "asdf";
  }];

  srcs = [ mime-types ];
}
