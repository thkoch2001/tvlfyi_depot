{ pkgs, lib, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "readyset";
  version = "beta-2023-08-23";
  src = pkgs.fetchFromGitHub {
    owner = "readysettech";
    repo = "readyset";
    rev = version;
    sha256 = "080pxcf2wly81pc8h6pzsy2r68b07j922n9lm017sk6v4ma21hp7";
  };

  patches = [
    ./fix-for-stable-rust.patch
  ];

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "consulrs-0.1.0" = "140axg4p2smwmlip49wa88d9bf78xik5i2m8a3v6bgiqjzadsxrg";
      "eui48-1.1.0" = "1jdwn4miya864n29s0bn42dyg0mm56rjfnkm19j2xa4j8iy7qxqd";
      "librocksdb-sys-0.11.0+8.1.1" = "19syj9gqba4lj56mdqja1mf64flb8qpfy0a4d4kff345hiq7bizd";
      "mysql_async-0.31.3" = "0bx05av3p0xlan3pzl8bb17n6xj0vb74fjyhznx7dpb5pmhym0pq";
      "nperf-core-0.1.1" = "1pqflrshac1gs6h2qk84c75yqzy7xmz38jkrnym1qm7d3xkkanrd";
      "opentelemetry-0.18.0" = "1na1qyhrabd4bxzabh9wxmk84q7qhsrh948ycx1abhr0m66wmpv1";
      "postgres-0.19.4" = "1phx5hi4pgjqqvf6hzi8hr21j3jb4navxq8jrgb5av4glsx0nasz";
      "rustify-0.5.2" = "0af87grq3nwi8hxnmxnmwkzwhkjr43b6sr4vlhx365988banc1lb";
    };
  };

  cargoSha256 = lib.fakeSha256;

  cargoBuildFlags = [
    "--bin readyset"
    "--bin readyset-server"
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
  ];

  buildInputs = with pkgs; [
    cmake
    cyrus_sasl
    llvmPackages.bintools
    llvmPackages.clang
    llvmPackages.libclang.lib
    libffi
    lz4
    libxml2
    zstd
    ncurses
    openssl
  ];

  doCheck = false;

  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib/libclang.so";
  BINDGEN_EXTRA_CLANG_ARGS = "-isystem ${pkgs.llvmPackages.libclang.lib}/lib/clang/${lib.getVersion pkgs.clang}/include";

  # Allow nightly features
  RUSTC_BOOTSTRAP = 1;

  # Tell the ReadySet builder about thee version
  RELEASE_VERSION = version;
  BUILDKITE_COMMIT = "e166f174866eec13442dec0c739566f6de7a6141";
}
