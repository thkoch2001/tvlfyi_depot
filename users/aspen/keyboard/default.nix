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
      pkgsCross.avr.buildPackages.gcc
      avrlibc
      avrdude
    ];

    AVR_CFLAGS = [
      "-isystem ${avrlibc}/avr/include"
      "-L${avrlibc}/avr/lib/avr5"
      # GCC 12 has improved array-bounds warnings, failing the build of QMK.
      # Newer versions of the firmware would work probably, but they heavily
      # altered the build system, so it is non-trivial. Backporting the patch
      # that fixes it seems difficult – the next change to the offending matrix.c
      # after the pinned qmkSource commit is
      # https://github.com/qmk/qmk_firmware/commit/11c308d436180974b7719ce78cdffdd83a1302c0
      # which heavily changes the way the code works.
      #
      # TODO(grfn): address this properly
      "-Wno-error=array-bounds"
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

  meta.ci.targets = [ "layout" ];
}
