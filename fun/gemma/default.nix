{ depot, pkgs, ... }:

let
  inherit (pkgs) cacert iana-etc libredirect stdenv runCommandNoCC writeText;
  elmPackages = pkgs.elmPackages_0_18;

  frontend = stdenv.mkDerivation {
    name = "gemma-frontend.html";
    src = ./frontend;
    buildInputs = [ cacert iana-etc elmPackages.elm ];

    # The individual Elm packages this requires are not packaged and I
    # can't be bothered to do that now, so lets open the escape hatch:
    outputHashAlgo = "sha256";
    outputHash = "000xhds5bsig3kbi7dhgbv9h7myacf34bqvw7avvz7m5mwnqlqg7";

    phases = [ "unpackPhase" "buildPhase" ];
    buildPhase = ''
      export NIX_REDIRECTS=/etc/protocols=${iana-etc}/etc/protocols \
        LD_PRELOAD=${libredirect}/lib/libredirect.so

      export SYSTEM_CERTIFICATE_PATH=${cacert}/etc/ssl/certs

      mkdir .home && export HOME="$PWD/.home"
      elm-make --yes Main.elm --output $out
    '';
  };

  injectFrontend = writeText "gemma-frontend.lisp" ''
    (in-package :gemma)
    (setq *static-file-location* "${runCommandNoCC "frontend" {} ''
      mkdir -p $out
      cp ${frontend} $out/index.html
    ''}/")
  '';
in depot.nix.buildLisp.program {
  name = "gemma";

  deps = with pkgs.lisp; [
    cl-json
    cl-prevalence
    hunchentoot
    local-time
  ];

  srcs = [
    ./src/gemma.lisp
    injectFrontend
  ];
}
