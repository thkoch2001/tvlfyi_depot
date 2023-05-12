{ pkgs, lib, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "ast-grep";
  version = "0.4.1";

  src = pkgs.fetchFromGitHub {
    owner = "ast-grep";
    repo = "ast-grep";
    rev = version;
    hash = "sha256:06vw42jbkajll49l1cfywl2li9gr5s0pnzhlrmwj1wjmq8c16vph";
  };

  # The Cargo.lock file in the repository is inconsistent and can not
  # be used. We work around this using a patch.
  cargoHash = "sha256:0dffhprnfz2sxzxar00xfambgn4xrwv1s79pfp0d649a2551cmiv";
  cargoPatches = [
    ./cargo-lock.patch
  ];

  meta = with lib; {
    description = "A fast and polyglot tool for code searching, linting, rewriting at large scale";
    homepage = "https://ast-grep.github.io/";
    license = licenses.mit;
    maintainers = [ maintainers.tazjin ];
  };
}
