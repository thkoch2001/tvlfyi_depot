{ pkgs, ... }:

pkgs.python3Packages.py3status.overridePythonAttrs (old: rec {
  name = "${pname}-${old.version}";
  pname = "py3status-glittershark";
  src = pkgs.fetchFromGitHub {
    owner = "glittershark";
    repo = "py3status";
    rev = "f243be1458cdabd5a7524adb76b5db99006c810c";
    sha256 = "0ffmv91562yk0wigriw4d5nfg2b32wqx8x78qvdqkawzvgbwrwvl";
  };
})
