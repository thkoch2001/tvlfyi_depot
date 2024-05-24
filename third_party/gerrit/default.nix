{ depot, pkgs, ... }:

let
  bazelRunScript = pkgs.writeShellScriptBin "bazel-run" ''
    yarn config set cache-folder "$bazelOut/external/yarn_cache"
    export HOME="$bazelOut/external/home"
    mkdir -p "$bazelOut/external/home"
    exec /bin/bazel "$@"
  '';
  bazelTop = pkgs.buildFHSUserEnv {
    name = "bazel";
    targetPkgs = pkgs: [
      (pkgs.bazel_5.override { enableNixHacks = true; })
      pkgs.jdk17_headless
      pkgs.zlib
      pkgs.python3
      pkgs.curl
      pkgs.nodejs
      pkgs.yarn
      pkgs.git
      bazelRunScript
    ];
    runScript = "/bin/bazel-run";
  };
  bazel = bazelTop // { override = x: bazelTop; };
  version = "3.9.1";
in
pkgs.lib.makeOverridable pkgs.buildBazelPackage {
  pname = "gerrit";
  inherit version;

  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/gerrit";
    rev = "620a819cbf3c64fff7a66798822775ad42c91d8e";
    branchName = "v${version}";
    sha256 = "sha256:1mdxbgnx3mpxand4wq96ic38bb4yh45q271h40jrk7dk23sgmz02";
    fetchSubmodules = true;
  };

  patches = [
    ./0001-Syntax-highlight-nix.patch
    ./0002-Syntax-highlight-rules.pl.patch
    ./0003-Add-titles-to-CLs-over-HTTP.patch
  ];

  bazelTargets = [ "release" "api-skip-javadoc" ];
  inherit bazel;

  bazelFlags = [
    "--repository_cache="
    "--disk_cache="
  ];

  removeRulesCC = false;
  fetchConfigured = true;

  fetchAttrs = {
    sha256 = "sha256-lsb9T2vIcEMlN21YnY6v9vv+W/lynvkXGYc+ZM0oJFI=";
    preBuild = ''
      rm .bazelversion
    '';

    installPhase = ''
      runHook preInstall

      # Remove all built in external workspaces, Bazel will recreate them when building
      rm -rf $bazelOut/external/{bazel_tools,\@bazel_tools.marker}
      rm -rf $bazelOut/external/{embedded_jdk,\@embedded_jdk.marker}
      rm -rf $bazelOut/external/{local_config_cc,\@local_config_cc.marker}
      rm -rf $bazelOut/external/{local_*,\@local_*.marker}

      # Clear markers
      find $bazelOut/external -name '@*\.marker' -exec sh -c 'echo > {}' \;

      # Remove all vcs files
      rm -rf $(find $bazelOut/external -type d -name .git)
      rm -rf $(find $bazelOut/external -type d -name .svn)
      rm -rf $(find $bazelOut/external -type d -name .hg)

      # Removing top-level symlinks along with their markers.
      # This is needed because they sometimes point to temporary paths (?).
      # For example, in Tensorflow-gpu build:
      #sha256:06bmzbcb9717s4b016kcbn8nr9pgaz04i8bnzg7ybkbdwpl8vxvv"; platforms -> NIX_BUILD_TOP/tmp/install/35282f5123611afa742331368e9ae529/_embedded_binaries/platforms
      find $bazelOut/external -maxdepth 1 -type l | while read symlink; do
        name="$(basename "$symlink")"
        rm -rf "$symlink" "$bazelOut/external/@$name.marker"
      done

      # Patching symlinks to remove build directory reference
      find $bazelOut/external -type l | while read symlink; do
        new_target="$(readlink "$symlink" | sed "s,$NIX_BUILD_TOP,NIX_BUILD_TOP,")"
        rm "$symlink"
        ln -sf "$new_target" "$symlink"
      done

      echo '${bazel.name}' > $bazelOut/external/.nix-bazel-version

      # Gerrit fixups:
      # Normalize permissions on .yarn-{tarball,metadata} files
      test -d $bazelOut/external/yarn_cache && find $bazelOut/external/yarn_cache \( -name .yarn-tarball.tgz -or -name .yarn-metadata.json \) -exec chmod 644 {} +

      mkdir $bazelOut/_bits/
      find . -name node_modules -prune -print | while read d; do
        echo "$d" "$(dirname $d)"
        mkdir -p $bazelOut/_bits/$(dirname $d)
        cp -R "$d" "$bazelOut/_bits/$(dirname $d)/node_modules"
      done

      # The yarn cache is produced in this fixed-output derivation, so that in
      # the build derivation all packages are available without network access.
      # `polymer-bridges` is a local package, and yarn copies it to the cache
      # along with a random UUID, making it non-deterministic. The easiest fix
      # is just to delete this particular package from the cache, since as a
      # local package, it will be available without network access in the build
      # derivation even without the cache.
      rm -r "$bazelOut"/external/yarn_cache/v6/npm-polymer-bridges-1.0.0-*

      (cd $bazelOut/ && tar czf $out --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner external/ _bits/)

      runHook postInstall
    '';
  };

  buildAttrs = {
    preConfigure = ''
      rm .bazelversion

      [ "$(ls -A $bazelOut/_bits)" ] && cp -R $bazelOut/_bits/* ./ || true
    '';
    postPatch = ''
      # Disable all errorprone checks, since we might be using a different version.
      sed -i \
        -e '/-Xep:/d' \
        -e '/-XepExcludedPaths:/a "-XepDisableAllChecks",' \
        tools/BUILD
    '';
    installPhase = ''
      mkdir -p "$out"/webapps/ "$out"/share/api/
      cp bazel-bin/release.war "$out"/webapps/gerrit-${version}.war
      unzip bazel-bin/api-skip-javadoc.zip -d "$out"/share/api
    '';

    nativeBuildInputs = with pkgs; [
      unzip
    ];
  };

  passthru = {
    # A list of plugins that are part of the gerrit.war file.
    # Use `java -jar gerrit.war ls | grep -Po '(?<=plugins/)[^.]+' | sed -e 's,^,",' -e 's,$,",' | sort` to generate that list.
    plugins = [
      "codemirror-editor"
      "commit-message-length-validator"
      "delete-project"
      "download-commands"
      "gitiles"
      "hooks"
      "plugin-manager"
      "replication"
      "reviewnotes"
      "singleusergroup"
      "webhooks"
    ];
  };

  meta.ci.targets = [ "deps" ];
}
