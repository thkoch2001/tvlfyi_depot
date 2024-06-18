{ pkgs, lib, ... }:
let
  pkgsCross = (import
    (pkgs.applyPatches {
      name = "nixpkgs-patched";
      src = pkgs.path;
      patches = [
        (pkgs.fetchpatch {
          url = "https://github.com/NixOS/nixpkgs/commit/de9a49c390e6793af604c9a05f8c7015aff32903.patch";
          hash = "sha256-r/K3z/cVuwtuTdmqnAZ63gaSiH4BW2SFR4YA/oYgJao=";
        })
        (pkgs.fetchpatch {
          url = "https://github.com/NixOS/nixpkgs/commit/9555aee82eb332e65edee057bbe1f9f5a5083295.patch";
          hash = "sha256-9Pa7KjEVgTbpXBtDG9G+PNr1tmqrPLl7dNHIG+PnrYw=";
        })
        (pkgs.fetchpatch {
          url = "https://github.com/NixOS/nixpkgs/commit/33d0aed3c8e53b42026d76790f0dbb699e6930ae.patch";
          hash = "sha256-aucNkLajS+W6jTcoQ1VT5gOn06uFmARNEz7rrdgEsnk=";
        })
        ../patches/0001-buildRustCrate-use-buildPlatform-s-library-extension.patch
      ];
    })
    { }).pkgsCross.wasm32-unknown-none;
in
(pkgsCross.callPackage ./Cargo.nix { }).rootCrate.build.overrideAttrs (old: {
  installPhase = ''
    ${lib.getExe pkgs.pkgsBuildBuild.wasm-bindgen-cli} \
      --target web \
      --out-dir $out \
      --out-name ${old.crateName} \
      --no-typescript \
      target/lib/${old.crateName}-*.wasm

      cp src/*.html $out/
  '';
})

