# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "effe6290559136faba5591a115e56c2b30210329";
    hash = "sha256:0kam9rqjk96brvh15wj3h3vm2sqnr5pckz91az2ida5617d5gp9v";
  };
in
depot.third_party.naersk.buildPackage {
  inherit src;

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkgconfig
  ];

  cargoBuildOptions = x: x ++ [
    "-p"
    "josh"
    "-p"
    "josh-proxy"
    "-p"
    "josh-ui"
  ];

  overrideMain = x: {
    patches = [ ./0001-josh-proxy-Always-require-authentication-when-pushin.patch ];

    nativeBuildInputs = (x.nativeBuildInputs or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/josh-proxy --prefix PATH : "${pkgs.git}/bin"
    '';
  };
}
