{ pkgs, ... }:

with pkgs;

let avrlibc = pkgsCross.avr.libcCross; in

rec {
  qmkSource = fetchgit {
    url = "https://github.com/qmk/qmk_firmware";
    rev = "ab1650606c36f85018257aba65d9c3ff8ec42e71";
    sha256 = "1k59flkvhjzmfl0yz9z37lqhvad7m9r5wy1p1sjk5274rsmylh79";
    fetchSubmodules = true;
  };

  layout = stdenv.mkDerivation rec {
    name = "ergodox_ez_grfn.hex";

    src = qmkSource;

    buildInputs = [
      dfu-programmer
      dfu-util
      diffutils
      git
      python3
      pkgsCross.avr.buildPackages.binutils
      pkgsCross.avr.buildPackages.gcc8
      avrlibc
      avrdude
    ];

    AVR_CFLAGS = [
      "-isystem ${avrlibc}/avr/include"
      "-L${avrlibc}/avr/lib/avr5"
    ];

    AVR_ASFLAGS = AVR_CFLAGS;

    patches = [ ./increase-tapping-delay.patch ];

    postPatch = ''
      mkdir keyboards/ergodox_ez/keymaps/grfn
      cp ${./keymap.c} keyboards/ergodox_ez/keymaps/grfn/keymap.c
    '';

    buildPhase = ''
      make ergodox_ez:grfn
    '';

    installPhase = ''
      cp ergodox_ez_grfn.hex $out
    '';
  };

  flash = writeShellScript "flash.sh" ''
    ${teensy-loader-cli}/bin/teensy-loader-cli \
      -v \
      --mcu=atmega32u4 \
      -w ${layout}
  '';

  meta.targets = [ "layout" ];
}
