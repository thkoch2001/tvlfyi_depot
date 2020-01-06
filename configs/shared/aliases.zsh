# Applications
#   java:       jv
#   tmux:       t
#   $EDITOR:    e
#   vim:        v
#   GnuPG:      gpg
#   blaze:      bz
#   borgcfg:    br
#   piper:      pi
#   pass:       ps
#   pastebin:   pb
#   pacman:     pm
#   codesearch: cs
#   git:        g
#   mercurial:  hg
#   aptitude:   apt
#   chrome:     c
#   elixir:     ex
#   haskell:    hk
#   wifi:       wf
#   piper:      pp
#   g4:         pp
#   g4d:        pp
#   cci:        circleci
#
# Supported qualifiers:
#   hidden:      h
#   ignore-case: i
#
# Supported verbs:
#   source:  s
#   install: i
#   test:    t
#   build:   b
#   list:    ls
#   shell:   sh

# Misc
alias c="xclip -selection clipboard -i"
alias p="xclip -selection clipboard -o"
alias cp=cp_dwim
alias mdd=mkdir_cd
alias mdp='mkdir --parents'
alias ls="exa              --sort=type"
alias ll="exa --long       --sort=type"
alias la="exa --long --all --sort=type"
alias lorem="echo Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
alias e="emacsclient --no-wait --create-frame"
alias cat="bat --theme=TwoDark"
alias j='fasd_cd -d' # to emulate autojump; my muscle memory is hardened here
alias vim=nvim # prefer neovim to vim
alias di=dired
alias chrome=google-chrome
alias btctl=bluetoothctl
alias rg='rg --ignore-case'
alias rgh='rg --hidden' # By default, rg skips hidden files
alias fdh='fd --hidden' # By default, rg skips hidden files
alias define=sdcv # uses stardict to lookup a word
alias intellij='nohup /opt/intellij-ce-stable/bin/idea.sh >/dev/null 2>&1 &'
alias tpr='tput reset'
alias nordvpn='sudo openvpn /etc/openvpn/ovpn_tcp/us3559.nordvpn.com.tcp.ovpn' # connects to the nordvpn servers in USA
alias perms='ls -ld' # list the permissions of a directory
alias please='sudo $(fc -ln -1)'
alias plz=please # for those keystroke-conscience folks
alias rmrf='rm -rf' # sometimes the dash is just too much...
alias open=xdg-open
alias o=open
alias simple_vim='vim -u ~/.config/nvim/simple.vim' # vim without a zero-dependency vimrc
alias stopx='sudo service lightdm stop' # stop X server session
alias next_wallpaper="emacsclient --eval '(wallpaper/next)'" # Cycles forwards one wallpaper.
alias prev_wallpaper="emacsclient --eval '(wallpaper/prev)'" # Cycles backwards one wallpaper.

# filesystem
alias files=laf
alias dirs=lad
alias links=lal

# device / power mgt
alias off='shutdown now' # TODO: Consider using `systemctl poweroff`
alias suspend='systemctl suspend'
alias hibernate='systemctl hibernate'

# pass
alias pscp='pass show --clip'

# nmcli
# NOTE: check out `tldr nmcli` for more information
alias wfls='nmcli device wifi' # list available wifi network
alias wfc='nmcli device wifi connect' # connect to a Wifi network

# Gnome
alias na=nautilus # Gnome's graphical file browser. Useful to click and dragging files into emails

# Tmux
alias tls='tmux list-sessions'
alias ta='tmux attach' # NOTE: `t` works for this as well
alias td='tmux detach' # prefer using the KBD for this (i.e. <leader>d)

# Chrome
alias cssh='chrome --app-id=pnhechapfaindjhompbnflcldabbghjo' # Secure Shell
alias crd='chrome --app-id=gbchcmhmhahfdphkhkmpfmihenigjmpp'  # Chrome Remote Desktop

# Dropbox
alias drst='dropbox.py status'

# Docker
alias dk="docker"
alias dkps="docker ps"
alias dkpsa="docker ps -a"
alias dkrm="docker rm"
alias dkrmi="docker rmi"
alias dkrd="docker run -d"
alias dki="docker images"

# Java
alias jvsh='CLASSPATH=$(fd \\.jar$ ~pro/jars | tr \\n :) jshell'

# Elixir
alias m="mix"
alias mc="mix compile"
alias mcf="mix compile --force"
alias ism="iex -S mix"
alias tism="MIX_ENV=test iex -S mix"
alias mdg="mix deps.get"
alias mdu="mix deps.update"
alias mdup="mix docker.up"
alias repl_ex='dkish elixir iex' # depends on the docker aliases

# Clojure
alias repl_clj='dkish clojure lein repl'

# GPG
alias gpged='gpg --edit-key wpcarro@gmail.com'
alias gpge='gpg --encrypt'
alias gpgd='gpg --decrypt'
alias gpgls='gpg --list-keys'

# Git
alias g="git"
alias glp='git log --graph --pretty=format:"%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset" --abbrev-commit --date=relative'
alias git="hub"
alias gprom="git pull --rebase origin master"
alias gca="git commit --amend"
alias gcan="git commit --amend --no-edit"
alias gpf="git push --force"
alias gpff="git push --force --no-verify"
alias gds="git diff --staged"
alias gfx="git commit --fixup"
alias gsh="git show"
alias gbm="git branch --merged"
alias gwip="git add . && git commit -m wip"
alias gpr="git pull-request"
alias gst="git status && hub pr list" # git status and list open hub PRs (if is a GH repository)

# Mercurial
# The attempt here is to map my well-known, existing `git` aliases to their
# Mercurial counterparts. Some may map 1:1, others may be like putting a square
# peg into a round hole. I will try and use my best judgement in these cases
# while erring on the side of unifying the two APIs.
alias hgst='PAGER="" hg status'
alias hglp='hg xl'
alias hgp='hg uploadchain' # this is like `git push`
alias hga='hg add'
alias hgc='hg commit'
alias hgcan='hg amend' # like `git commit --amend --no-edit'
alias hgpr='hg mail -r . -m' # this may be similar to `hub pull-request`
alias hgd='hg diff'
alias hgsh='hg export'
alias hgco='hg update'
alias hgls='hg citc --list' # should have different output from `pils`
alias hgrc='hg rebase --continue'
alias hgra='hg rebase --abort'
alias hgconflicts="hg resolve --list 'set:unresolved()'" # much like `gconflicts`
alias hgrm='hg citc -d' # delete a CitC client created with Fig

# Piper
alias ppls='g4 listclients | sed "s/^Client wpcarro://" | sed "s/:[0-9]*:citc.*$//g"'
alias pprm='p4 citc -d -f' # warning this will forcefully delete a CitC client even if contains pending changes

# Haskell
alias sb="stack build"
alias se="stack exec --"
alias sc="stack clean"
# alias st="stack test" # blocks suckless-terminal
alias haddocks='open "$(stack path --local-doc-root)/index.html"'
alias repl_hk='dkish haskell ghci'

# Kubernetes
alias kc="kubectl"
alias kpods="kubectl get pods"
alias knodes="kubectl get nodes"
alias kdeploys="kubectl get deployments"
alias kdns="kubectl get ing"
alias kedit="kubectl edit deployments "
alias kswitch="gcloud container clusters get-credentials "

# Nix
alias nq="nix_introspect"
alias nsh="nix-shell"
alias nshp="nix-shell --pure"
alias nr="nix repl"
alias ni='nix-env --install'
alias nrm='nix-env --uninstall'
alias nls='nix-env --query'
alias nrs='sudo nixos-rebuild switch'

# Aptitude (apt)
alias apti='sudo apt-get install --assume-yes'
alias aptrm='sudo apt remove'

# Pacman
alias pmi='sudo pacman -S --noconfirm'
alias pms='pacman -Ss'
alias pmrm='sudo pacman -Rs'

# couple the e* aliases to the <leader>e* kbds in vim
alias ev='e ~/.config/nvim/init.vim'
alias ee='e ~/.emacs.d/init.el'
alias ez='e ~/.zshrc'
alias ea='e ~/aliases.zsh'
alias ef='e ~/functions.zsh'
alias el='e ~/variables.zsh'
alias ex='e ~/.Xresources'
alias em='e ~/.tmux.conf'
# TODO: consider DRYing this up with `e`. Unfortunately, `sudo` won't support
# aliases.
alias en='sudo ALTERNATE_EDITOR=nvim emacsclient --no-wait --create-frame /etc/nixos/configuration.nix'
alias er='e ~/Dropbox/dotfiles/README.md'

# couple the s* aliases to the <leader>s* kbds in vim
alias sz='source ~/.zshrc'
alias sa='source ~/aliases.zsh'
alias sf='source ~/functions.zsh'
alias sl='source ~/variables.zsh'
alias sx='xrdb ~/.Xresources'
alias sm='tmux source-file ~/.tmux.conf'
alias sn='sudo nixos-rebuild switch'

# CircleCI
alias cci='circleci local'
alias ccijob='circleci local execute --job'

# Google aliases
alias bzb='blaze build'
alias bzt='blaze test --test_output=all'
alias br='borgcfg'
alias pils='p4 listclients'
alias pirm='p4 citc -d'
alias pb=/google/src/head/depot/eng/tools/pastebin
alias pbc='pb --private --title $(date +${date_fmt})| tee >(c && chrome $(p))' # create a private gPaste from your clipboard's content; open the result in a browser
alias pbcp='p | pb --private --title $(date +${date_fmt})| tee >(c && chrome $(p))' # create a private gPaste from your clipboard's content; open the result in a browser
alias pbls='$BROWSER https://paste.googleplex.com/$(whoami)'
alias flagpick=/google/data/ro/users/sk/skaushik/www/public-tools/flagpick
alias jaze=/google/data/ro/projects/devtools/javascript/jaze
alias aclcheck=/google/data/ro/projects/ganpati/aclcheck
alias g3python=/google/data/ro/projects/g3python/g3python
