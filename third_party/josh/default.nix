# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "4a70f2914260066b2db44f4cabe9b80acdd556af";
    sha256 = "0gwn3daj24538h5ddksvnyhq4lyjh0br9k49jhbd7r1sm6ly2cwl";
  };
in depot.third_party.naersk.buildPackage {
  inherit src;

  buildInputs = with pkgs; [ libgit2 openssl pkgconfig ];

  cargoBuildOptions = x: x ++ [ "-p" "josh" "-p" "josh-proxy" "-p" "josh-ui" ];

  overrideMain = x: {
    patches = [ ./0001-replace-mentions-of-master-with-canon.patch ];
  };
}
