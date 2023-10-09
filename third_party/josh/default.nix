# https://github.com/josh-project/josh
{ depot, pkgs, ... }:

let
  # TODO(sterni): switch to pkgs.josh as soon as that commit is released
  rev = "1586eab06284ce668779c87f00a1fb5fa9763be0";
  src = pkgs.fetchFromGitHub {
    owner = "josh-project";
    repo = "josh";
    inherit rev;
    hash = "sha256-94QrHcVHiEMCpBZJ5sghwtVNLNm4gdG8X85OetoGRD0=";
  };


  naersk = pkgs.callPackage depot.third_party.sources.naersk {
    inherit (pkgs) rustc cargo;
  };
  version = "git-${builtins.substring 0 8 rev}";
in
naersk.buildPackage {
  pname = "josh";
  inherit src version;
  JOSH_VERSION = version;

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
    preBuild = x.preBuild or "" + ''
      echo 'debug = true' >> Cargo.toml
    '';

    nativeBuildInputs = (x.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/josh-proxy --prefix PATH : "${pkgs.git}/bin"
    '';
  };
}
