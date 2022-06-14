{ pkgs, ... }:

with pkgs;

rustPlatform.buildRustPackage rec {
  pname = "cargo-hakari";
  version = "0.9.13";

  src = fetchFromGitHub {
    owner = "facebookincubator";
    repo = "cargo-guppy";
    rev = "cargo-hakari-${version}";
    sha256 = "11ds2zryxdd6rvszkpphb0xnfg7rqisg6kixrwyiydjrm5rdjg9d";
  };

  cargoSha256 = "0b2hjyak5v4m3g5zjk2q8bdb4iv3015qw1rmhpclv4cv48lcmdbb";

  buildAndTestSubdir = "tools/cargo-hakari";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    openssl
  ];
}
