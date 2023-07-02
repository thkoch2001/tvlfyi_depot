# https://github.com/josh-project/josh
{ depot, pkgs, ... }:

let
  # TODO(sterni): switch to pkgs.josh as soon as that commit is released
  rev = "c0a170a756dd5e63268673086218c0ce7bf18bdc";
  src = pkgs.fetchFromGitHub {
    owner = "josh-project";
    repo = "josh";
    inherit rev;
    hash = "sha256:0rsf65fq9xm3qj77ig3s4wmmgm50jhvwrknr839hipjj5lj4x1hp";
  };


  rust = third_party.nixpkgs.rust-bin.stable."1.69.0".rustc;
  naersk = pkgs.callPackage depot.third_party.sources.naersk {
    rustc = rust.rustc;
    cargo = rust.cargo;
  };
in
naersk.buildPackage {
  inherit src;
  JOSH_VERSION = "git-${builtins.substring 0 8 rev}";

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkg-config
  ];

  dontStrip = true;
  cargoBuildOptions = x: x ++ [
    "-p"
    "josh-filter"
    "-p"
    "josh-proxy"
  ];

  overrideMain = x: {
    preBuild = ''
      echo 'debug = true' >> Cargo.toml
    '';

    nativeBuildInputs = (x.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/josh-proxy --prefix PATH : "${pkgs.git}/bin"
    '';
  };
}
