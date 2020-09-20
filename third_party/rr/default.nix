{ pkgs, ... }:

pkgs.originals.rr.overrideAttrs(_: {
  src = pkgs.fetchFromGitHub {
    owner = "mozilla";
    repo = "rr";
    rev = "8fc7d2a09a739fee1883d262501e88613165c1dd";
    sha256 = "0avq5lv082z2sasggfn2awnfrh08cr8f0i9iw1dnrcxa6pc3bi9k";
    fetchSubmodules = false;
  };

  # Workaround as documented on https://github.com/mozilla/rr/wiki/Zen
  postInstall = ''
    cp $src/scripts/zen_workaround.py $out/bin/rr_zen_workaround
  '';
})
