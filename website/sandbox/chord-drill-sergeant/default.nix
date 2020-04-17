{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "learn-piano-chords";
  src = ./.;
  buildInputs = with pkgs; [
    elmPackages.elm
    nodejs
  ];
  outputHashMode = "recursive";
  outputHashAlgo = "sha256";
  outputHash = "0diya7q8ird56jsbf2p49fyvldsay6m9z251zr2rq1i4qs7idy0j";
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    export NIX_REDIRECTS=/etc/protocols=${pkgs.iana-etc}/etc/protocols
    export LD_PRELOAD=${pkgs.libredirect}/lib/libredirect.so
    export SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs

    mkdir -p $out
    cp index.html $out
    elm make src/Main.elm --optimize --output=$out/elm.js
    npx tailwindcss build index.css -o $out/output.css
  '';
}
