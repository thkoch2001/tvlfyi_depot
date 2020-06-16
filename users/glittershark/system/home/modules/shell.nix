{ config, lib, pkgs, ... }:
let
  shellAliases = rec {
    # NixOS stuff
    hms = "home-manager switch";
    nor = "sudo nixos-rebuild switch";
    nrs = nor;
    nrb = "sudo nixos-rebuild boot";
    ncg = "nix-collect-garbage";
    vihome = "vim ~/.config/nixpkgs/home.nix && home-manager switch";
    virc = "vim ~/code/system/home/modules/shell.nix && home-manager switch && source ~/.zshrc";
    visystem = "sudo vim /etc/nixos/configuration.nix && sudo nixos-rebuild switch";

    # Nix
    ns = "nix-shell";
    nb = "nix build -f .";
    nc = "nix copy --to https://nix.urbinternal.com";
    "nc." = "nix copy -f . --to https://nix.urbinternal.com";

    # Docker and friends
    "dcu" = "docker-compose up";
    "dcud" = "docker-compose up -d";
    "dc" = "docker-compose";
    "dcr" = "docker-compose restart";
    "dclf" = "docker-compose logs -f";
    "dck" = "docker";
    "dockerclean" = "dockercleancontainers && dockercleanimages";
    "dockercleanimages" = "docker images -a --no-trunc | grep none | awk '{print \$$3}' | xargs -L 1 -r docker rmi";
    "dockercleancontainers" = "docker ps -a --no-trunc| grep 'Exit' | awk '{print \$$1}' | xargs -L 1 -r docker rm";

    # Directories
    stck = "dirs -v";
    b= "cd ~1";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";

    # Aliases from old config
    "http" = "http --style solarized";
    "grep" = "grep $GREP_OPTIONS";
    "bak" = "~/bin/backup.sh";
    "xmm" = "xmodmap ~/.Xmodmap";
    "asdflkj" = "asdf";
    "asdf" = "asdfghjkl";
    "asdfghjkl" = "echo \"Having some trouble?\"";
    "ift" = "sudo iftop -i wlp3s0";
    "first" = "awk '{print \$$1}'";
    "cmt" = "git log --oneline | fzf-tmux | awk '{print \$$1}'";
    "workmon" = "xrandr --output DP-2 --pos 1440x900 --primary";
    "vi" = "vim";
    "adbdev" = "adb devices";
    "adbcon" = "adb connect $GNEX_IP";
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
in {
  home.packages = with pkgs; [
    zsh
    autojump
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    LS_COLORS = "no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:";
    BROWSER = "firefox";
    BAT_THEME = "ansi-light";
  };

  programs.bash = {
    enable = true;
    inherit shellAliases;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    autocd = true;

    inherit shellAliases;

    history = rec {
      save = 100000;
      size = save;
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

      export RPS1=""
      autoload -U promptinit; promptinit
      prompt pure

      if [[ "$TERM" == "dumb" ]]; then
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        unfunction precmd
        unfunction preexec
        export PS1='$ '
      fi
    '';
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
