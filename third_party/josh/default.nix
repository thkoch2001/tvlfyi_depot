# https://github.com/josh-project/josh
{ depot, pkgs, ... }:

let
  rev = "fc857afda2c1536234e3bb1983c518a1abf63d25";
  src = pkgs.fetchFromGitHub {
    owner = "josh-project";
    repo = "josh";
    inherit rev;
    hash = "sha256:16ch7al7xfyjipgqh2n7grj985fv713mhi8y5bixb736vsad9q3w";
  };
in
depot.third_party.naersk.buildPackage {
  inherit src;
  JOSH_VERSION = "git-${builtins.substring 0 8 rev}";

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkg-config
  ];

  cargoBuildOptions = x: x ++ [
    "-p"
    "josh-filter"
    "-p"
    "josh-proxy"
  ];

  overrideMain = x: {
    nativeBuildInputs = (x.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/josh-proxy --prefix PATH : "${pkgs.git}/bin"
    '';
  };
}
