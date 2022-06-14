{ pkgs, ... }:

with pkgs;

rustPlatform.buildRustPackage rec {
  pname = "cargo-nextest";
  version = "0.9.14";

  src = fetchFromGitHub {
    owner = "nextest-rs";
    repo = "nextest";
    rev = "cargo-nextest-${version}";
    sha256 = "0nc8xz90m03yydj7zafjgciv4vxwzbz814pnjdi49ddkr4q20sc3";
  };

  cargoSha256 = "0rcsh573qryllgc199ah2dbrn1xcp215q2xkjb3f4ps757m7scnm";

  cargoTestFlags = [
    "--"
    "--skip"
    "tests_integration::test_relocated_run"
    "--skip"
    "tests_integration::test_run"
    "--skip"
    "tests_integration::test_run_after_build"
  ];
}
