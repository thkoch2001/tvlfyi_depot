# Nix helpers for projects under //tvix
{ lib
, pkgs
, depot
, ...
}:

let
  # Run crate2nix generate in the current working directory, then
  # format the generated file with depotfmt.
  crate2nix-generate = pkgs.writeShellScriptBin "crate2nix-generate" ''
    ${pkgs.crate2nix}/bin/crate2nix generate --all-features
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  '';

in

(import ./. {
  tvix-protobufs = depot.tvix.proto;
  src = depot.third_party.gitignoreSource ./.;
})
  // {
  inherit crate2nix-generate;

  # Run crate2nix generate, ensure the output doesn't differ afterwards
  # (and doesn't fail).
  #
  # Currently this re-downloads every crate every time
  # crate2nix-check (but not crate2nix) is built.
  # TODO(amjoseph): be less wasteful with bandwidth.
  #
  crate2nix-check =
    let
      outputHashAlgo = "sha256";
    in
    pkgs.stdenv.mkDerivation {
      inherit src;

      # Important: we include the hash of the Cargo.lock file and
      # Cargo.nix file in the derivation name.  This forces the FOD
      # to be rebuilt/reverified whenever either of them changes.
      name = "tvix-crate2nix-check-" +
      (builtins.substring 0 8 (builtins.hashFile "sha256" ./Cargo.lock)) +
      "-" +
      (builtins.substring 0 8 (builtins.hashFile "sha256" ./Cargo.nix));

      nativeBuildInputs = with pkgs; [ git cacert cargo ];
      buildPhase = ''
        export CARGO_HOME=$(mktemp -d)

        # The following command can be omitted, in which case
        # crate2nix-generate will run it automatically, but won't show the
        # output, which makes it look like the build is somehow "stuck" for a
        # minute or two.
        cargo metadata > /dev/null

        # running this command counteracts depotfmt brokenness
        git init

        ${crate2nix-generate}/bin/crate2nix-generate

        # technically unnecessary, but provides more-helpful output in case of error
        diff -ur Cargo.nix ${src}/Cargo.nix

        # the FOD hash will check that the (re-)generated Cargo.nix matches the committed Cargo.nix
        cp Cargo.nix $out
      '';

      # This is an FOD in order to allow `cargo` to perform network access.
      outputHashMode = "flat";
      inherit outputHashAlgo;
      outputHash = builtins.hashFile outputHashAlgo ./Cargo.nix;
      env.SSL_CERT_FILE = "${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt";
    };
}
