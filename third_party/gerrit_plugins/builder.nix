{ depot, pkgs, ... }:
{
  buildGerritBazelPlugin = {
    name,
    src,
    depsOutputHash,
    overlayPluginCmd ? ''
      cp -R "${src}" "$out/plugins/${name}"
    '',
  }: ((depot.third_party.gerrit.override {
    name = "${name}.jar";

    src = pkgs.runCommandLocal "${name}-src" {} ''
      cp -R "${depot.third_party.gerrit.src}" "$out"
      chmod +w "$out/plugins"
      ${overlayPluginCmd}
    '';

    bazelTarget = "//plugins/${name}";
  }).overrideAttrs (super: {
    deps = super.deps.overrideAttrs (superDeps: {
      outputHash = depsOutputHash;
    });
    installPhase = ''
      cp "bazel-bin/plugins/${name}/${name}.jar" "$out"
    '';
  }));
}
