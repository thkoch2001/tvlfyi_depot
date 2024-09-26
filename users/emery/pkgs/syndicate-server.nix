{ pkgs, ... }:

let
  inherit (pkgs)
    rustPlatform
    rust-bin
    fetchFromGitea
    pkg-config
    openssl
    ;
in

rustPlatform.buildRustPackage rec {
  pname = "syndicate-server";
  version = "0.46.0";
  src = fetchFromGitea {
    domain = "git.syndicate-lang.org";
    owner = "syndicate-lang";
    repo = "syndicate-rs";
    rev = "${pname}-v${version}";
    sha256 = "sha256-bTteZIlBSoQ1o5shgd9NeKVvEhZTyG3i2zbeVojWiO8=";
  };
  cargoHash = "sha256-SIpdFXTk6MC/drjCLaaa49BbGsvCMNbPGCfTxAlCo9c=";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl ];

  RUSTC_BOOTSTRAP = 1;

  meta = {
    description = "Syndicate broker server";
    homepage = "https://git.syndicate-lang.org/syndicate-lang/syndicate-rs/";
    mainProgram = "syndicate-server";
  };
}
