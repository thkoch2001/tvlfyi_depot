{ config, lib, pkgs, ... }:

let

  clj2nix = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "hlolli";
    repo = "clj2nix";
    rev = "3ab3480a25e850b35d1f532a5e4e7b3202232383";
    sha256 = "1lry026mlpxp1j563qs13nhxf37i2zpl7lh0lgfdwc44afybqka6";
  }) {};

  pg-dump-upsert = pkgs.buildGoModule rec {
    pname = "pg-dump-upsert";
    version = "165258deaebded5e9b88f7a0acf3a4b7350e7bf4";

    src = pkgs.fetchFromGitHub {
      owner = "tomyl";
      repo = "pg-dump-upsert";
      rev = version;
      sha256 = "1an4h8jjbj3r618ykjwk9brii4h9cxjqy47c4c8rivnvhimgf4wm";
    };

    vendorSha256 = "1a5fx6mrv30cl46kswicd8lf5i5shn1fykchvbnbhdpgxhbz6qi4";
  };

in

with lib;

{
  imports = [
    ./lib/zshFunctions.nix
    ./development/kube.nix
    # TODO(grfn): agda build is broken in the nixpkgs checkout
    # ./development/agda.nix
    ./development/rust.nix
  ];

  home.packages = with pkgs; [
    jq
    yq
    gitAndTools.hub
    gitAndTools.tig
    gitAndTools.gh
    shellcheck
    httpie
    entr
    gnumake
    inetutils
    tokei
    jsonnet
    ngrok

    gdb
    lldb
    hyperfine
    config.lib.depot.third_party.clang-tools

    clj2nix
    clojure
    leiningen
    clj-kondo

    pg-dump-upsert

    nodePackages.prettier
  ] ++ optionals (stdenv.isLinux) [
    julia
    valgrind
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    userEmail = "root@gws.fyi";
    userName  = "Griffin Smith";
    ignores = [
      "*.sw*"
      ".classpath"
      ".project"
      ".settings/"
      ".dir-locals.el"
      ".stack-work-profiling"
      ".projectile"
    ];
    extraConfig = {
      github.user = "glittershark";
      merge.conflictstyle = "diff3";
      rerere.enabled = "true";
    };

    delta = {
      enable = true;
      options = {
        theme = "Solarized (light)";
        hunk-style = "plain";
        commit-style = "box";
      };
    };
  };

  home.file.".psqlrc".text = ''
    \set QUIET 1
    \timing
    \set ON_ERROR_ROLLBACK interactive
    \set VERBOSITY verbose
    \x auto
    \set PROMPT1 '%[%033[1m%]%M/%/%R%[%033[0m%]%# '
    \set PROMPT2 '...%# '
    \set HISTFILE ~/.psql_history- :DBNAME
    \set HISTCONTROL ignoredups
    \pset null [null]
    \unset QUIET
  '';

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
    '';
  };

  programs.zsh = {
    shellAliases = {
      # Git
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
      "gcc" = "gc canon";
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
      "grim" = "git fetch && git rebase -i --autostash origin/master";
      "grom" = "git fetch && git rebase --autostash origin/master";
      "groc" = "git fetch && git rebase --autostash origin/canon";
      "grc" = "git rebase --continue";
      "gcan" = "git commit --amend --no-edit";
      "grl" = "git reflog";

      # Haskell
      "crl" = "cabal repl";
      "cr" = "cabal run";
      "cnb" = "cabal new-build";
      "cob" = "cabal old-build";
      "cnr" = "cabal new-run";
      "cor" = "cabal old-run";
      "ho" = "hoogle";
    };

    functions = {
      gdelmerged = ''
      git branch --merged | egrep -v 'master' | tr -d '+ ' | xargs git branch -d
      '';
    };
  };
}
