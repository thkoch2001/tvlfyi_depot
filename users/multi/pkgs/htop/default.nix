{ pkgs, ... }:

let
  newVer = "3.0.2";

  newSrc = pkgs.fetchFromGitHub {
    owner = "htop-dev";
    repo = "htop";
    rev = "59ef15b2ad6037f40d7fe4207b2b59dd11b14b8b";
    sha256 = "0sirwfvqwwq2x2k25vd4k4cf9d5qv17yjizidxq4y5xfh2v0djmd";
  };
in
  with pkgs; htop.overrideAttrs
    ({ nativeBuildInputs ? [], ... }:
      {
        nativeBuildInputs = nativeBuildInputs ++ [ autoreconfHook ];
        src = newSrc;
        version = newVer;
      })
