{ depot, pkgs, ... }:

let
  detzip = depot.nix.buildGo.program {
    name = "detzip";
    srcs = [ ./detzip.go ];
  };
  bazelRunScript = pkgs.writeShellScriptBin "bazel-run" ''
    yarn config set cache-folder "$bazelOut/external/yarn_cache"
    export HOME="$bazelOut/external/home"
    mkdir -p "$bazelOut/external/home"
    exec /bin/bazel "$@"
  '';
  bazelTop = pkgs.buildFHSUserEnv {
    name = "bazel";
    targetPkgs = pkgs: [
      (pkgs.bazel.override { enableNixHacks = true; })
      detzip
      pkgs.jdk11_headless
      pkgs.zlib
      pkgs.python
      pkgs.curl
      pkgs.nodejs
      pkgs.yarn
      pkgs.git
      bazelRunScript
    ];
    runScript = "/bin/bazel-run";
  };
  bazel = bazelTop // { override = x: bazelTop; };
  version = "3.3.0-rc7-520-gdef99cd679";
in
pkgs.lib.makeOverridable pkgs.buildBazelPackage {
  pname = "gerrit";
  inherit version;

  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/gerrit";
    rev = "595e6db974449ff278709f5e87cd8d42199305fc";
    sha256 = "sha256:17mad9k41rvnzq91nr9w2lax6rdk7pgf20qc4chajay99faimhph";
    fetchSubmodules = true;
  };
  patches = [
    ./0001-Use-detzip-in-download_bower.py.patch
    ./0002-Syntax-highlight-nix.patch
    ./0003-Syntax-highlight-rules.pl.patch
    ./0004-Add-titles-to-CLs-over-HTTP.patch
    ./0005-When-using-local-fonts-always-assume-Gerrit-is-mount.patch
    ./0006-Always-use-Google-Fonts.patch
    ./0007-Keep-left-padding-on-account-chip-if-no-avatar-provi.patch
  ];

  bazelTarget = "release";
  inherit bazel;

  bazelFlags = [
    "--repository_cache="
    "--disk_cache="
  ];
  removeRulesCC = false;
  fetchConfigured = true;

  fetchAttrs = {
    sha256 = "sha256:16vm9kk7hqbk80y2frxv6yirhzn4aiyak5i7dxypbkvvqj880c7b";
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
      # Remove polymer-bridges and ba-linkify, they're in-repo
      rm -rf $bazelOut/external/yarn_cache/v6/npm-polymer-bridges-*
      rm -rf $bazelOut/external/yarn_cache/v6/npm-ba-linkify-*
      # Normalize permissions on .yarn-{tarball,metadata} files
      find $bazelOut/external/yarn_cache \( -name .yarn-tarball.tgz -or -name .yarn-metadata.json \) -exec chmod 644 {} +

      (cd $bazelOut/ && tar czf $out --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner external/)

      runHook postInstall
    '';
  };

  buildAttrs = {
    preConfigure = ''
      rm .bazelversion
    '';
    installPhase = ''
      mkdir -p "$out"/webapps/
      cp bazel-bin/release.war "$out"/webapps/gerrit-${version}.war
    '';
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
