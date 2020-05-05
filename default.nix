{ nixpkgs ? import <nixpkgs> {}
}:

with nixpkgs;

rec {
  qmkSource = fetchgit {
    url = "https://github.com/qmk/qmk_firmware";
    rev = "ab1650606c36f85018257aba65d9c3ff8ec42e71";
    sha256 = "1k59flkvhjzmfl0yz9z37lqhvad7m9r5wy1p1sjk5274rsmylh79";
    fetchSubmodules = true;
  };

  qmk = import "${qmkSource}/shell.nix" {
    avr = true;
    teensy = true;
    arm = false;
  };

  layout = stdenv.mkDerivation {
    name = "ergodox_ez_grfn.hex";

    src = qmkSource;

    inherit (qmk) buildInputs AVR_CFLAGS AVR_ASFLAGS;

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
}
