{ depot, pkgs, ... }:
let
  defaultOverlayPluginCmd = attrs: ''
      cp -R "${attrs.src}" "$out/plugins/${attrs.name}"
  '';
in
{
  buildGerritBazelPlugin = attrs@{
    overlayPluginCmd ? defaultOverlayPluginCmd attrs,
    ...
  }: ((depot.third_party.gerrit.override rec {
    name = "${attrs.name}.jar";

    src = pkgs.runCommandLocal "${attrs.name}-src" {} ''
      cp -R "${depot.third_party.gerrit.src}" "$out"
      chmod +w "$out/plugins"
      ${overlayPluginCmd}
    '';

    bazelTarget = "//plugins/${attrs.name}";
  }).overrideAttrs (super: {
    deps = super.deps.overrideAttrs (superDeps: {
      outputHash = attrs.depsOutputHash;
    });
    installPhase = ''
      cp "bazel-bin/plugins/${attrs.name}/${attrs.name}.jar" "$out"
    '';
  }));
}
