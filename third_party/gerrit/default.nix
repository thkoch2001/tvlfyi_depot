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
      pkgs.jdk11_headless
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
  version = "3.7.0-rc4";
in
pkgs.lib.makeOverridable pkgs.buildBazelPackage {
  pname = "gerrit";
  inherit version;

  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/gerrit";
    rev = "3e445c7833c4acf49f1171fe4c82ceb32e93c780";
    branchName = "v${version}";
    sha256 = "sha256:002aw2bfifyla66v8khyiq4m9qj6ahs6r1dzb5kjk8xqpf6c6q9p";
    fetchSubmodules = true;
  };

  patches = [
    ./0001-Syntax-highlight-nix.patch
    ./0002-Syntax-highlight-rules.pl.patch
    ./0003-Add-titles-to-CLs-over-HTTP.patch
  ];

  bazelTarget = "release api-skip-javadoc";
  inherit bazel;

  bazelFlags = [
    "--repository_cache="
    "--disk_cache="
  ];

  removeRulesCC = false;
  fetchConfigured = true;

  fetchAttrs = {
    sha256 = "sha256:1z0j09mz7ycf0kmiinv4879jvx25rp6sn2da2hqw8a9rf5rf6ias";
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
      # platforms -> NIX_BUILD_TOP/tmp/install/35282f5123611afa742331368e9ae529/_embedded_binaries/platforms
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
      find $bazelOut/external/yarn_cache \( -name .yarn-tarball.tgz -or -name .yarn-metadata.json \) -exec chmod 644 {} +

      mkdir $bazelOut/_bits/
      find . -name node_modules -prune -print | while read d; do
        echo "$d" "$(dirname $d)"
        mkdir -p $bazelOut/_bits/$(dirname $d)
        cp -R "$d" "$bazelOut/_bits/$(dirname $d)/node_modules"
      done

      (cd $bazelOut/ && tar czf $out --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner external/ _bits/)

      runHook postInstall
    '';
  };

  buildAttrs = {
    preConfigure = ''
      rm .bazelversion

      cp -R $bazelOut/_bits/* ./
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
}
