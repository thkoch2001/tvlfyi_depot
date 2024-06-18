{ pkgs, lib, ... }:

(pkgs.pkgsCross.wasm32-unknown-none.callPackage ./Cargo.nix { }).rootCrate.build.overrideAttrs (oldAttrs: {
  installPhase = ''
    ${lib.getExe pkgs.wasm-bindgen-cli} \
      --target web \
      --out-dir $out \
      --out-name ${oldAttrs.crateName} \
      --no-typescript \
      target/lib/${oldAttrs.crateName}-${oldAttrs.metadata}.wasm

      mv src/*.html $out
  '';
})

