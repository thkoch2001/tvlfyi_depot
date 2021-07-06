{ config, lib, pkgs, ... }:

with pkgs;

let
  libuiohook = stdenv.mkDerivation rec {
    pname = "libuiohook";
    version = "1.1";
    src = fetchFromGitHub {
      owner = "kwhat";
      repo = "libuiohook";
      rev = version;
      sha256 = "1isfxn3cfrdqq22d3mlz2lzm4asf9gprs7ww2xy9c3j3srk9kd7r";
    };

    preConfigure = ''
      ./bootstrap.sh
    '';

    nativeBuildInputs = [ pkg-config ];
    buildInputs = [
      libtool autoconf automake
      x11
      xorg.libXtst
      xorg.libXinerama
      xorg.libxkbfile
      libxkbcommon
    ];
  };

  obs-input-overlay = stdenv.mkDerivation rec {
    pname = "obs-input-overlay";
    version = "4.8";
    src = fetchFromGitHub {
      owner = "univrsal";
      repo = "input-overlay";
      rev = "v${version}";
      sha256 = "1dklg0dx9ijwyhgwcaqz859rbpaivmqxqvh9w3h4byrh5pnkz8bf";
      fetchSubmodules = true;
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ obs-studio libuiohook ];

    postPatch = ''
      sed -i CMakeLists.txt \
        -e '2iinclude(${obs-studio.src}/cmake/Modules/ObsHelpers.cmake)' \
        -e '2ifind_package(LibObs REQUIRED)'
    '';

    cmakeFlags = [
      "-Wno-dev"
    ];
  };
in
{
  home.packages = [
    obs-studio
    obs-input-overlay
  ];

  xdg.configFile."obs-studio/plugins/input-overlay/bin/64bit/input-overlay.so".source =
    "${obs-input-overlay}/lib/obs-plugins/input-overlay.so";
  xdg.configFile."obs-studio/plugins/input-overlay/data".source =
    "${obs-input-overlay}/share/obs/obs-plugins/input-overlay";
}
