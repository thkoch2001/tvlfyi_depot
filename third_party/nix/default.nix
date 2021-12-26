args@{
  depot ? (import ../.. {})
, pkgs ? depot.third_party.nixpkgs
, lib
, buildType ? "release"
, ...
}:

let
  aws-s3-cpp = pkgs.aws-sdk-cpp.override {
    apis = ["s3" "transfer"];
    customMemoryManagement = false;
  };

  src = let
    srcDir = ./.;
    # create relative paths for all the sources we are filtering
    asRelative = path:
      let
        srcS = toString srcDir;
        pathS = toString path;
      in
        if ! lib.hasPrefix srcS pathS then
          throw "Path is outside of the working directory."
        else
        lib.removePrefix srcS pathS;

  in builtins.filterSource (path: type:
    # Strip out .nix files that are in the root of the repository.  Changing
    # the expression of tvix shouldn't cause a rebuild of tvix unless really
    # required.
    !(dirOf (asRelative path) == "/" && lib.hasSuffix ".nix" path) &&

    # remove the proto files from the repo as those are compiled separately
    !(lib.hasPrefix "src/proto" (asRelative path)) &&

    # ignore result symlinks
    !(type == "symlink" && lib.hasPrefix "result" (baseNameOf path))
  ) srcDir;

  # Proto generation in CMake is theoretically possible, but that is
  # very theoretical - this does it in Nix instead.
  protoSrcs = pkgs.runCommand "nix-proto-srcs" {} ''
    export PROTO_SRCS=${./src/proto}
    mkdir -p $out/libproto
    ${depot.third_party.protobuf}/bin/protoc -I=$PROTO_SRCS \
      --cpp_out=$out/libproto \
      --plugin=protoc-gen-grpc=${depot.third_party.grpc}/bin/grpc_cpp_plugin \
        --grpc_out=$out/libproto \
        $PROTO_SRCS/*.proto
  '';

  # Derivation for busybox that just has the `busybox` binary in bin/, not all
  # the symlinks, so cmake can find it
  busybox = pkgs.runCommand "busybox" {} ''
    mkdir -p $out/bin
    cp ${pkgs.busybox}/bin/busybox $out/bin
  '';

in lib.fix (self: pkgs.fullLlvm11Stdenv.mkDerivation {
  pname = "tvix";
  version = "2.3.4";
  inherit src;

  nativeBuildInputs = with pkgs; [
    bison
    clang-tools_11
    cmake
    libxml2
    libxslt
    pkgconfig
    (import ./clangd.nix pkgs)
  ];

  # TODO(tazjin): Some of these might only be required for native inputs
  buildInputs = (with pkgs; [
    aws-s3-cpp
    brotli
    bzip2
    c-ares
    curl
    editline
    flex
    libseccomp
    libsodium
    systemd.dev
    openssl
    sqlite
    xz
  ]) ++ (with depot.third_party; [
    abseil_cpp
    glog
    grpc
    protobuf
  ]);

  doCheck = false;
  doInstallCheck = true;

  # Preserve debug symbols, for core dumps + other live debugging
  dontStrip = true;

  installCheckInputs = with depot.third_party; [
    gtest
    pkgs.fd
    rapidcheck
  ];

  propagatedBuildInputs = with pkgs; [
    boost
  ];

  configurePhase = ''
    mkdir build
    cd build
    cmake .. \
      -DCMAKE_INSTALL_PREFIX=$out \
      -DCMAKE_BUILD_TYPE=RelWithDebInfo \
      -DCMAKE_FIND_USE_SYSTEM_PACKAGE_REGISTRY=OFF \
      -DCMAKE_FIND_USE_PACKAGE_REGISTRY=OFF \
      -DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=ON
  '';

  installCheckPhase = ''
    export NIX_DATA_DIR=$out/share
    export NIX_TEST_VAR=foo # this is required by a language test
    make test
  '';

  preBuild = ''
    if [ -n "$NIX_BUILD_CORES" ]; then
      makeFlags+="-j$NIX_BUILD_CORES "
      makeFlags+="-l$NIX_BUILD_CORES "
    fi
  '';

  # Forward the location of the generated Protobuf / gRPC files so
  # that they can be included by CMake.
  NIX_PROTO_SRCS = protoSrcs;

  # Work around broken system header include flags in the cxx toolchain.
  LIBCXX_INCLUDE = "${pkgs.llvmPackages_11.libcxx}/include/c++/v1";

  SANDBOX_SHELL="${pkgs.busybox}/bin/busybox";

  # Install the various symlinks to the Nix binary which users expect
  # to exist.
  postInstall = ''
    ln -s $out/bin/nix $out/bin/nix-build
    ln -s $out/bin/nix $out/bin/nix-channel
    ln -s $out/bin/nix $out/bin/nix-collect-garbage
    ln -s $out/bin/nix $out/bin/nix-copy-closure
    ln -s $out/bin/nix $out/bin/nix-env
    ln -s $out/bin/nix $out/bin/nix-hash
    ln -s $out/bin/nix $out/bin/nix-instantiate
    ln -s $out/bin/nix $out/bin/nix-prefetch-url
    ln -s $out/bin/nix $out/bin/nix-shell
    ln -s $out/bin/nix $out/bin/nix-store

    mkdir -p $out/libexec/nix
    ln -s $out/bin/nix $out/libexec/nix/build-remote

    # configuration variables for templated files
    export storedir=/nix/store
    export localstatedir=/nix/var
    export bindir=$out/bin

    mkdir -p $out/lib/systemd/system
    substituteAll \
      ${src}/misc/systemd/nix-daemon.service.in \
      $out/lib/systemd/system/nix-daemon.service
    substituteAll \
      ${src}/misc/systemd/nix-daemon.socket.in \
      $out/lib/systemd/system/nix-daemon.socket

    mkdir -p $out/etc/profile.d
    substituteAll \
      ${src}/scripts/nix-profile.sh.in $out/etc/profile.d/nix.sh
    substituteAll \
      ${src}/scripts/nix-profile-daemon.sh.in $out/etc/profile.d/nix-daemon.sh
  '';

  # TODO(tazjin): integration test setup?
  # TODO(tazjin): docs generation?

  passthru = {
    build-shell = self.overrideAttrs (up: rec {
      run_clang_tidy = pkgs.writeShellScriptBin "run-clang-tidy" ''
        test -f compile_commands.json || (echo "run from build output directory"; exit 1) || exit 1
        ${pkgs.jq}/bin/jq < compile_commands.json -r 'map(.file)|.[]' | grep -v '/generated/' | ${pkgs.parallel}/bin/parallel ${pkgs.clang-tools}/bin/clang-tidy -p compile_commands.json $@
      '';

      installCheckInputs = up.installCheckInputs ++ [run_clang_tidy];

      shellHook = ''
        export NIX_DATA_DIR="${toString depot.path}/third_party"
        export NIX_TEST_VAR=foo
      '';
    });

    # Ensure formatting is coherent,
    # but do this in parallel to the main build because:
    #  - (in favor of building this after tvix)
    #    tests run so that developers get all the useful feedback
    #  - (in favor of building this before tvix)
    #    if the formatting is broken, and this build was submitted to CI
    #    it would be a good idea to get this feedback rather sooner than later
    #  - we don't want builds to differ between local and CI runs
    checkfmt = pkgs.fullLlvm11Stdenv.mkDerivation {
      name = "tvix-checkfmt";
      inherit src;
      nativeBuildInputs = with pkgs; [ clang-tools_11 fd ];
      SANDBOX_SHELL = "${pkgs.busybox}/bin/busybox";

      buildPhase = ''
        set -e
        runHook preBuild
        fd . $src -e hh -e cc | xargs clang-format --dry-run --Werror
        touch $out
        runHook postBuild
      '';
    };

    test-vm = import ./test-vm.nix args;
  };

  meta = {
    targets = [
      "checkfmt"
    ];
  };
})
