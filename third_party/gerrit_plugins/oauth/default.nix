{ depot, pkgs, ... }@args:

let inherit (import ../builder.nix args) buildGerritBazelPlugin;
in buildGerritBazelPlugin rec {
  name = "oauth";
  depsOutputHash =
    "sha256:0ww88msym6zr5z86k5az1kmw3hv8d9giniwkii4lwnzf3kc5qnrx";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/oauth";
    rev = "4aa7322db5ec221b2419e12a9ec7af5b8c66659c";
    sha256 = "1szra3pjl0axf4a7k96flpk7rhfvp37rdxay4gbglh939gzbba88";
  };
  overlayPluginCmd = ''
    chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
    cp -R "${src}" "$out/plugins/${name}"
    cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
  '';

  # The code in the OAuth repo expects CAS to return oauth2 access tokens as urlencoded.
  # Our version of CAS returns them as JSON instead.
  postPatch = ''
    pushd plugins/oauth
    patch -p1 <${./cas-6x.patch}
    popd
  '';
}
