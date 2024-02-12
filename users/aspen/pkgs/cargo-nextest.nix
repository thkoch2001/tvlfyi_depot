{ pkgs, ... }:

with pkgs;

rustPlatform.buildRustPackage rec {
  pname = "cargo-nextest";
  version = "0.9.36";

  src = fetchFromGitHub {
    owner = "nextest-rs";
    repo = "nextest";
    rev = "cargo-nextest-${version}";
    sha256 = "1g40r38bqmdhc0dy07pj27vkc64d3fw6v5z2vwn82xld2h9dg7w2";
  };

  cargoSha256 = "1g862azgkn3xk3v3chs8hv1b1prj1pq2vfzbhcx6ir9l00kv6gcv";

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
