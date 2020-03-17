{ config, pkgs, ... }:

let
  briefcase = import <briefcase> {};
in {
  home = {
    packages = with pkgs; [
      bat
      exa
      ripgrep
      fd
      pass
      tokei
      nmap
      tldr
      diskus
      jq
      pup
    ];
    keyboard = {
      options = [
        # Swap Caps-Lock and Escape
        "remove Lock = Caps_Lock"
        "keysym Caps_Lock = Escape"
      ];
    };
    sessionVariables = {
      BROWSER = "google-chrome";
      EDITOR = "emacsclient";
      ALTERNATE_EDITOR = "vim";
    };
    stateVersion = "19.09";
  };

  ##############################################################################
  # Programs
  ##############################################################################

  programs.home-manager = {
    enable = true;
    path = builtins.toPath ~/home-manager;
  };

  programs.git = {
    enable = true;
    package = briefcase.utils.wrapNonNixProgram {
      path = "/usr/bin/git";
      as = "git";
    };
    userName = "William Carroll";
    userEmail = "wpcarro@gmail.com";
    aliases = {
      today = "! git log --date=relative --since=00:00:00 --all --no-merges --oneline --author=\"$(git config --get user.email)\"";
      yday = "! git log --since=yesterday.midnight --until=today.midnight --oneline --author=\"$(git config --get user.email)\"";
      changed-files = "! git --no-pager diff --name-only $(current_branch) $(git merge-base $(current_branch) master)";
      conflicts = "! git --no-pager diff --name-only --diff-filter=U";
      unstage = "reset HEAD --";
    };
    extraConfig = {
      push.default = "current";
      rebase = {
        autosquash = true;
        autostash = true;
      };
      rerere.enabled = true;
    };
  };

  programs.gpg = {
    enable = true;
    settings = {
      keyserver = "hkp://pgp.mit.edu";
    };
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      desktop = {
        user = "wpcarro";
        hostname = "zeno.lon.corp.google.com";
      };
      socrates = {
        user = "wpcarro";
        hostname = "84.92.33.141";
      };
    };
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      c = "xclip -selection clipboard -i";
      p = "xclip -selection clipboard -o";
      cat = "bat --theme='Monokai Extended Light'";
      rgh = "rg --hidden";
      fdh = "fd --hidden";
      tpr = "tput reset";
      ls = "exa --sort=type";
      ll = "exa --long --sort=type";
      la = "exa --long --all --sort=type";
      gst = "git status";
      gsh = "git show HEAD";
      gpf = "git push --force-with-lease";
      gd = "git diff";
    };
    shellAbbrs = {
      sys = "systemctl";
      sysst = "systemctl status";
      sysu = "systemctl --user";
      sysust = "systemctl --user status";
    };
    promptInit = builtins.readFile ../fish/prompt.fish;
    functions = {
      ptree = {
        body = ''
          for pid in (pgrep $argv[1])
            pstree -s -p $pid
          end
        '';
      };
    };
  };

  programs.fzf = rec {
    enable = true;
    defaultCommand = "fd --hidden --follow --exclude '.git'";
    fileWidgetCommand = defaultCommand;
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.autorandr = {
    enable = true;
    profiles = let
      # To get these values, I ran `xrandr --props` and searched for
      # 'EDID:'. While these are long and a bit unsightly, I cannot think of a
      # desirable workaround, so I'm going to leave them for now.
      laptop = "00ffffffffffff000daed71400000000151b0104a51f117802ee95a3544c99260f505400000001010101010101010101010101010101363680a0703820402e1e240035ad10000018000000fe004e3134304843452d474e320a20000000fe00434d4e0a202020202020202020000000fe004e3134304843452d474e320a2000e2";
      hdmi4k = "00ffffffffffff001e6d085b6d4a0400061c0103803c2278ea3035a7554ea3260f50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c4720556c7472612048440a200150020330714d902220050403020161605d5e5f230907076d030c001000b83c20006001020367d85dc401788003e30f0003023a801871382d40582c450058542100001a565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000ad";
    in {
      mobile = {
        fingerprint = {
          eDP1 = laptop;
          HDMI1 = hdmi4k;
        };
        config = {
          eDP1 = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            rate = "59.93";
          };
          HDMI1 = {
            enable = false;
          };
        };
      };
      docked = {
        fingerprint = {
          eDP1 = laptop;
          HDMI1 = hdmi4k;
        };
        config = {
          eDP1 = {
            enable = false;
          };
          HDMI1 = {
            enable = true;
            primary = true;
            mode = "3840x2160";
            rate = "30.00";
          };
        };
      };
    };
  };

  ##############################################################################
  # Services
  ##############################################################################

  xsession = {
    enable = true;
    windowManager.command = "dbus-launch --exit-with-session wpcarros-emacs";
  };

  # Filter blue light from screen after sunset.
  services.redshift = {
    enable = true;
    latitude = "51.49";
    longitude = "-0.18";
    package = briefcase.utils.wrapNonNixProgram {
      path = "/usr/bin/redshift";
      as = "redshift";
    };
    # Disable the fading animation.
    extraOptions = [ "-r" ];
  };

  # Hide the cursor during X sessions after 1 second.
  services.unclutter.enable = true;

  # Support mouseless workflows.
  services.keynav.enable = true;

  services.lorri.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 8 * 60 * 60; # 8 hours
    maxCacheTtl = 8 * 60 * 60;
  };
}
