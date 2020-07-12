{ pkgs, ... }:

with pkgs.stdenv.lib;
let
  inherit (pkgs) stdenv fetchFromGitHub;

  srt = pkgs.srt.overrideAttrs (super: {
    patchPhase = ''
      sed -i 's/typedef SRT_ATR_DEPRECATED/SRT_ATR_DEPRECATED typedef/g' srtcore/srt.h
    '';
  });
  ffmpeg = pkgs.ffmpeg_3_4.overrideAttrs (super: {
    pname = "${super.pname}-ovenmediaengine";
    src = fetchFromGitHub {
      owner = "Airensoft";
      repo = "FFmpeg";
      rev = "142b4bb64b64e337f80066e6af935a68627fedae";  # ome/3.4
      sha256 = "0fla3940q3z0c0ik2xzkbvdfvrdg06ban7wi6y94y8mcipszpp11";
    };
  });
in
stdenv.mkDerivation rec {
  pname = "oven-media-engine";
  version = "0.10.4";

  src = fetchFromGitHub {
    owner = "AirenSoft";
    repo = "OvenMediaEngine";
    rev = "v${version}";
    sha256 = "15lrlynsldlpa21ryzccf5skgiih6y5fc9qg0bfqh557wnnmml6w";
  };

  sourceRoot = "source/src";
  makeFlags = "release CONFIG_LIBRARY_PATHS= CONFIG_PKG_PATHS= GLOBAL_CC=$(CC) GLOBAL_CXX=$(CXX) GLOBAL_LD=$(CXX)";
  enableParallelBuilding = true;

  nativeBuildInputs = with pkgs; [ bc pkgconfig perl ];
  buildInputs = with pkgs; [ openssl srt zlib ffmpeg libvpx libopus srtp jemalloc ];

  postUnpack = ''
    cp -R $sourceRoot/.. $NIX_BUILD_TOP/build
    chmod -R u+rwX $NIX_BUILD_TOP/build
    export sourceRoot=$NIX_BUILD_TOP/build/src
    echo "source root is now $sourceRoot"
  '';

  preBuild = ''
    patchShebangs core/colorg++
    patchShebangs core/colorgcc
    patchShebangs projects/main/update_git_info.sh

    sed -i -e '/^CC =/d' -e '/^CXX =/d' -e '/^AR =/d' projects/third_party/pugixml-1.9/scripts/pugixml.make
  '';

  installPhase = ''
    install -Dm0755 bin/RELEASE/OvenMediaEngine $out/bin/OvenMediaEngine
    install -Dm0644 ../misc/conf_examples/Origin.xml $out/share/examples/origin_conf/Server.xml
    install -Dm0644 ../misc/conf_examples/Logger.xml $out/share/examples/origin_conf/Logger.xml
    install -Dm0644 ../misc/conf_examples/Edge.xml $out/share/examples/edge_conf/Server.xml
    install -Dm0644 ../misc/conf_examples/Logger.xml $out/share/examples/edge_conf/Logger.xml
  '';

  meta = {
    description = "Open-source streaming video service with sub-second latency";
    homepage    = "https://ovenmediaengine.com/";
    licenses    = licenses.gpl2;
    platforms   = platforms.all;
  };
}
