{
  depot,
  lib,
  pkgs,
  ...
}:
{
  buildGerritBazelPlugin =
    {
      name,
      src,
      depsOutputHash,
      overlayPluginCmd ? ''
        cp -R "${src}" "$out/plugins/${name}"
      '',
      postPatch ? "",
      patches ? [ ],
    }:
    (
      (depot.third_party.gerrit.override {
        name = "${name}.jar";

        src = pkgs.runCommandLocal "${name}-src" { } ''
          cp -R "${depot.third_party.gerrit.src}" "$out"
          chmod +w "$out/plugins"
          ${overlayPluginCmd}
        '';

        bazelTargets = [ "//plugins/${name}" ];
      }).overrideAttrs
      (super: {
        deps = super.deps.overrideAttrs (superDeps: {
          outputHash = depsOutputHash;
        });
        installPhase = ''
          cp "bazel-bin/plugins/${name}/${name}.jar" "$out"
        '';
        postPatch = ''
          ${super.postPatch or ""}
          pushd "plugins/${name}"
          ${lib.concatMapStringsSep "\n" (patch: ''
            patch -p1 < ${patch}
          '') patches}
          popd
          ${postPatch}
        '';
      })
    );
}
