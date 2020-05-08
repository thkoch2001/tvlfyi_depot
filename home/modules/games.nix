{ config, lib, pkgs, ... }:

with pkgs;

let

  df-orig = dwarf-fortress-packages.dwarf-fortress-original;

  df-full = (dwarf-fortress-packages.dwarf-fortress-full.override {
    theme = null;
    enableIntro = false;
    enableFPS = true;
  });

  init = runCommand "init.txt" {} ''
    substitute "${df-orig}/data/init/init.txt" $out \
      --replace "[INTRO:YES]" "[INTRO:NO]" \
      --replace "[VOLUME:255]" "[VOLUME:0]" \
      --replace "[FPS:NO]" "[FPS:YES]"
  '';

  d_init = runCommand "d_init.txt" {} ''
    substitute "${df-orig}/data/init/d_init.txt" $out \
      --replace "[AUTOSAVE:NONE]" "[AUTOSAVE:SEASONAL]" \
      --replace "[AUTOSAVE_PAUSE:NO]" "[AUTOSAVE_PAUSE:YES]" \
      --replace "[INITIAL_SAVE:NO]" "[INITIAL_SAVE:YES]" \
      --replace "[EMBARK_WARNING_ALWAYS:NO]" "[EMBARK_WARNING_ALWAYS:YES]" \
      --replace "[VARIED_GROUND_TILES:YES]" "[VARIED_GROUND_TILES:NO]" \
      --replace "[SHOW_FLOW_AMOUNTS:NO]" "[SHOW_FLOW_AMOUNTS:YES]"
  '';

  df = runCommand "dwarf-fortress" {} ''
     mkdir -p $out/bin
     sed \
       -e '4icp -f ${init} "$DF_DIR/data/init/init.txt"' \
       -e '4icp -f ${d_init} "$DF_DIR/data/init/d_init.txt"' \
       < "${df-full}/bin/dwarf-fortress" >"$out/bin/dwarf-fortress"
     chmod +x $out/bin/dwarf-fortress
  '';

in {
  home.packages = [
    crawl
    df
  ];
}
