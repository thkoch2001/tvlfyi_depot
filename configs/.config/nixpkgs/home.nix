{ config, pkgs, ... }:

{
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

  ##############################################################################
  # Services
  ##############################################################################

  # Filter blue light from screen after sunset.
  services.redshift = {
    enable = true;
    latitude = "51.49";
    longitude = "-0.18";
    # The redshift from <nixpkgs> isn't working on gLinux.
    package = pkgs.writeShellScriptBin "redshift" ''
      exec /usr/bin/redshift "$@"
    '';
  };

  services.lorri.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 7200;
    maxCacheTtl = 7200;
  };
}
