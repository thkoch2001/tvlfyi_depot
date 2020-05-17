let pkgs = (import <nixpkgs> {}).third_party;
in with pkgs;

with import ./release-common.nix { inherit pkgs; };

clangStdenv.mkDerivation {
  name = "nix";

  buildInputs = buildDeps ++ propagatedDeps ++ tarballDeps ++ perlDeps;

  inherit configureFlags;

  enableParallelBuilding = true;

  installFlags = "sysconfdir=$(out)/etc";

  shellHook =
    ''
      export prefix=$(pwd)/inst
      configureFlags+=" --prefix=$prefix"
      PKG_CONFIG_PATH=$prefix/lib/pkgconfig:$PKG_CONFIG_PATH
      PATH=$prefix/bin:$PATH
    '';
}
