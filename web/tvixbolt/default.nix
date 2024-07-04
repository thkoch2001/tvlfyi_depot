{ pkgs, lib, depot, ... }:
let
  pkgsCross = pkgs.pkgsCross.wasm32-unknown-none;
in
(pkgsCross.callPackage ./Cargo.nix {
  defaultCrateOverrides = (depot.tvix.utils.defaultCrateOverridesForPkgs pkgsCross) // {
    tvixbolt = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };
  };
}).rootCrate.build.overrideAttrs (oldAttrs: {
  installPhase = ''
    ${lib.getExe pkgs.wasm-bindgen-cli} \
      --target web \
      --out-dir $out \
      --out-name ${oldAttrs.crateName} \
      --no-typescript \
      target/lib/${oldAttrs.crateName}-${oldAttrs.metadata}.wasm

      mv src/*.{html,css} $out
  '';

  passthru.serve = pkgs.writeShellScriptBin "tvixbolt-serve" ''
    ${lib.getExe pkgs.simple-http-server} \
        --index \
        --nocache \
        "$@" \
        ${depot.web.tvixbolt}
  '';
})

