{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
  ];

  home.stateVersion = "19.09";

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

  programs.fzf = rec {
    defaultCommand = "fd --hidden --follow --exclude '.git'";
    fileWidgetCommand = defaultCommand;
  };

  ##############################################################################
  # Services
  ##############################################################################

  services.lorri.enable = true;
}
