{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    zsh
    autojump
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    LS_COLORS = "no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:";
    TWITTER_WHOAMI = "glittershark1";
    BROWSER = "firefox";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    autocd = true;

    shellAliases = rec {
      # NixOS stuff
      hms = "home-manager switch";
      nor = "sudo nixos-rebuild switch";
      nrs = nor;
      vihome = "vim ~/.config/nixpkgs/home.nix && home-manager switch";
      virc = "vim ~/.config/nixpkgs/home/shell.nix && home-manager switch && source ~/.zshrc";

      # Nix
      ns = "nix-shell";

      # Aliases from old config
      stck = "dirs -v";
      b= "cd ~1";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
      "....." = "cd ../../../..";
      "http" = "http --style solarized";
      "grep" = "grep $GREP_OPTIONS";
      "bak" = "~/bin/backup.sh";
      "xmm" = "xmodmap ~/.Xmodmap";
      "asdflkj" = "asdf";
      "asdf" = "asdfghjkl";
      "asdfghjkl" = "echo \"Having some trouble?\"";
      "ift" = "sudo iftop -i wlp3s0";
      "rvpn" = "sudo systemctl restart openvpn@bldr-dev openvpn@lsvl-dev";
      "gne" = "gn edit";
      "gnf" = "gn find";
      "gnt" = "gn tag-list";
      "gnn" = "gn notebook-list";
      "mytl" = "t tl $TWITTER_WHOAMI";
      "first" = "awk '{print \$$1}'";
      "dcu" = "docker-compose up";
      "dc" = "docker-compose";
      "dck" = "docker";
      "dockerclean" = "dockercleancontainers && dockercleanimages";
      "dockercleanimages" = "docker images -a --no-trunc | grep none | awk '{print \$$3}' | xargs -L 1 -r docker rmi";
      "dockercleancontainers" = "docker ps -a --no-trunc| grep 'Exit' | awk '{print \$$1}' | xargs -L 1 -r docker rm";
      "cmt" = "git log --oneline | fzf-tmux | awk '{print \$$1}'";
      "workmon" = "xrandr --output DP-2 --pos 1440x900 --primary";
      "vi" = "vim";
      "awa" = "ssh aw2-admin.nomi.host";
      "dtf" = "cd ~/.dotfiles";
      "adbdev" = "adb devices";
      "adbcon" = "adb connect $GNEX_IP";
      "gwip" = "git add . && git commit -am wip";
      "gpr" = "g pull-request";
      "gcl" = "git clone";
      "grs" = "gr --soft";
      "grhh" = "grh HEAD";
      "grh" = "gr --hard";
      "gr" = "git reset";
      "gcb" = "gc -b";
      "gco" = "gc";
      "gcd" = "gc development";
      "gcm" = "gc master";
      "gc" = "git checkout";
      "gbg" = "git branch | grep";
      "gba" = "git branch -a";
      "gb" = "git branch";
      "gcv" = "git commit --verbose";
      "gci" = "git commit";
      "gm" = "git merge";
      "gdc" = "gd --cached";
      "gd" = "git diff";
      "gsl" = "git stash list";
      "gss" = "git show stash";
      "gsad" = "git stash drop";
      "gsa" = "git stash";
      "gst" = "gs";
      "gs" = "git status";
      "gg" = "gl --decorate --oneline --graph --date-order --all";
      "gl" = "git log";
      "gf" = "git fetch";
      "gur" = "gu --rebase";
      "gu" = "git pull";
      "gpf" = "gp -f";
      "gpa" = "gp --all";
      "gpu" = "git push -u origin \"$(git symbolic-ref --short HEAD)\"";
      "gp" = "git push";
      "ganw" = "git diff -w --no-color | git apply --cached --ignore-whitespace";
      "ga" = "git add";
      "gnp" = "git --no-pager";
      "g" = "git";
      "git" = "hub";
      "mpalb" = "mpc search album";
      "mpart" = "mpc search artist";
      "mps" = "mpc search";
      "mpa" = "mpc add";
      "mpt" = "mpc toggle";
      "mpl" = "mpc playlist";
      "dsstore" = "find . -name '*.DS_Store' -type f -ls -delete";
      "df" = "df -h";
      "fs" = "stat -f '%z bytes'";
      "ll" = "ls -al";
      "la" = "ls -a";
    };

    oh-my-zsh = {
      enable = true;

      plugins = [
        "battery"
        "colorize"
        "command-not-found"
        "github"
        "gitignore"
        "postgres"
        "systemd"
        "themes"
        "vi-mode"
      ];

      custom = "${pkgs.stdenv.mkDerivation {
        name = "oh-my-zsh-custom";
        unpackPhase = ":";
        installPhase = ''
          mkdir -p $out/themes
          mkdir -p $out/custom/plugins
          ln -s ${./pure.zsh-theme} $out/themes/pure.zsh-theme
        '';
      }}";

      theme = "pure";
    };

    plugins = [{
      name = "pure-theme";
      src = pkgs.fetchFromGitHub {
        owner = "sindresorhus";
        repo = "pure";
        rev = "0a92b02dd4172f6c64fdc9b81fe6cd4bddb0a23b";
        sha256 = "0l8jqhmmjn7p32hdjnv121xsjnqd2c0plhzgydv2yzrmqgyvx7cc";
      };
    }];

    initExtraBeforeCompInit = ''
      zstyle ':completion:*' completer _complete _ignored _correct _approximate
      zstyle ':completion:*' matcher-list \'\' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._- :]=** r:|=**' 'l:|=* r:|=*'
      zstyle ':completion:*' max-errors 5
      zstyle ':completion:*' use-cache yes
      zstyle ':completion::complete:grunt::options:' expire 1
      zstyle ':completion:*' prompt '%e errors'
      # zstyle :compinstall filename '~/.zshrc'
      autoload -Uz compinit
    '';

    initExtra = ''
      source ${./zshrc}
      source ${pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "7678a8a22780141617f809002eeccf054bf8f448";
        sha256 = "0xh4fbd54kvwwpqvabk8lpw7m80phxdzrd75q3y874jw0xx1a9q6";
      }}/zsh-syntax-highlighting.zsh
      source ${pkgs.autojump}/share/autojump/autojump.zsh
      source ${pkgs.fetchFromGitHub {
        owner = "chisui";
        repo = "zsh-nix-shell";
        rev = "a65382a353eaee5a98f068c330947c032a1263bb";
        sha256 = "0l41ac5b7p8yyjvpfp438kw7zl9dblrpd7icjg1v3ig3xy87zv0n";
      }}/nix-shell.plugin.zsh

      autoload -U promptinit; promptinit
      prompt pure

      [[ ! $IN_NIX_SHELL ]] && alsi -l
    '';
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
