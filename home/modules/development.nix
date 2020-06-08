{ config, lib, pkgs, ... }:

let

  clj2nix = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "hlolli";
    repo = "clj2nix";
    rev = "3ab3480a25e850b35d1f532a5e4e7b3202232383";
    sha256 = "1lry026mlpxp1j563qs13nhxf37i2zpl7lh0lgfdwc44afybqka6";
  }) {};

in

with lib;

{
  imports = [
    ./lib/zshFunctions.nix
    ./development/kube.nix
    ./development/urbint.nix
  ];

  home.packages = with pkgs; [
    jq
    yq
    gitAndTools.hub
    gitAndTools.tig
    shellcheck
    httpie
    entr
    gnumake
    inetutils
    loc

    clj2nix

    haskellPackages.Agda
    AgdaStdlib

    (import ../pkgs/clang-tools { inherit pkgs; })
  ] ++ optional (stdenv.isLinux) julia;

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
    };

    delta = {
      enable = true;
      options = [
        "--theme 'Solarized (light)'"
        "--hunk-style" "plain"
        "--commit-style" "box"
      ];
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

  programs.zsh.shellAliases = {
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
    "grim" = "git fetch && git rebase -i origin/master";
    "grc" = "git rebase --continue";
    "gcan" = "git commit --amend --no-edit";
    "grl" = "git reflog";

    # Haskell
    "cnb" = "cabal new-build";
    "cob" = "cabal old-build";
    "cnr" = "cabal new-run";
    "cor" = "cabal old-run";
    "ho" = "hoogle";
  };
}
