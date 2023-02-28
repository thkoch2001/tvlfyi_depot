# https://github.com/josh-project/josh
{ depot, pkgs, ... }:

let
  rev = "159d7187ea79104c5ff315fc5cc7e9d08f63c3b9";
  src = pkgs.fetchFromGitHub {
    # TODO: josh-project, after https://github.com/josh-project/josh/pull/1153
    owner = "tvlfyi";
    repo = "josh";
    inherit rev;
    hash = "sha256:1dkyj431wprl6g33dq9a44d0qrdj94dw8xj0jg6x41kzh0gvq05l";
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
