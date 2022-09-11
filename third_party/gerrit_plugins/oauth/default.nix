{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;
in
buildGerritBazelPlugin rec {
  name = "oauth";
  depsOutputHash = "sha256:14xw282ianq52y5cbcpxrlkfjjakcvh7igpkvs49hcgcb7v4rds8";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/oauth";
    rev = "f9bef7476bc99f7b1dc3fe2d52ec95cd7ac571dc";
    sha256 = "08wf50bz7ash37mzlrxfy7hvmjsf6s4ncpcw5969hs9hjvjfj4dz";
  };
  overlayPluginCmd = ''
    chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
    cp -R "${src}" "$out/plugins/${name}"
    cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
  '';
}
