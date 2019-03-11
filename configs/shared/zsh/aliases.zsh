# Applications
# dired:  di
# docker: dk

# Misc
alias c="xclip -selection clipboard -i"
alias p="xclip -selection clipboard -o"
alias md="mkdir_cd"
alias ls="exa"
alias ll="exa -l"
alias la="exa -la"
alias lorem="echo Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
alias e="emacsclient --no-wait"
alias cat="bat"
alias j='fasd_cd -d' # to emulate autojump; my muscle memory is hardened here
alias vim=nvim # prefer neovim to vim
alias links='find ~ -maxdepth 1 -type l -exec exa {} \;' # list all of the links in the home directory
alias di=dired
alias chrome=google-chrome
alias btctl=bluetoothctl
alias rg='rg --ignore-case'
alias rgh='rg --hidden' # By default, rg skips hidden files
alias fdh='fd --hidden' # By default, rg skips hidden files

# Gnome
alias na=nautilus # Gnome's graphical file browser. Useful to click and dragging files into emails

# Chrome
alias cssh='chrome --app-id=pnhechapfaindjhompbnflcldabbghjo' # Secure Shell
alias crd='chrome --app-id=gbchcmhmhahfdphkhkmpfmihenigjmpp'  # Chrome Remote Desktop

# Docker
alias dk="docker"
alias dkps="docker ps"
alias dkpsa="docker ps -a"
alias dkrm="docker rm"
alias dkrmi="docker rmi"
alias dkrit="docker run -it"
alias dkrd="docker run -d"
alias dki="docker images"

# Elixir
alias m="mix"
alias mc="mix compile"
alias mcf="mix compile --force"
alias ism="iex -S mix"
alias tism="MIX_ENV=test iex -S mix"
alias mdg="mix deps.get"
alias mdu="mix deps.update"
alias mdup="mix docker.up"

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
alias gcan="git commit --amend --no-edit"
alias gpf="git push --force"
alias gpff="git push --force --no-verify"
alias gds="git diff --staged"
alias gfx="git commit --fixup"
alias gsh="git show"
alias gbm="git branch --merged"
alias gwip="git add . && git commit -m wip"
alias gpr="git pull-request"

# Mercurial
# The attempt here is to map my well-known, existing `git` aliases to their
# Mercurial counterparts. Some may map 1:1, others may be like putting a square
# peg into a round hole. I will try and use my best judgement in these cases
# while erring on the side of unifying the two APIs.
alias hgst='hg status'
alias hglp='hg xl'
alias hgp='hg uploadchain' # this is like `git push`
alias hga='hg add'
alias hgc='hg commit'
alias hgcan='hg amend' # like `git commit --amend --no-edit'
alias hgpr='hg mail -r . -m' # this may be similar to `hub pull-request`
alias hgd='hg diff'
alias hgsh='hg export'
alias hgco='hg update'

# Haskell
alias sb="stack build"
alias se="stack exec --"
alias sc="stack clean"
alias st="stack test"
alias haddocks='open "$(stack path --local-doc-root)/index.html"'

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

# couple the e* aliases to the <leader>e* kbds in vim
alias ev='e ~/.config/nvim/init.vim'
alias ee='e ~/.emacs.d/init.el'
alias ez='e ~/.zshrc'
alias ea='e ~/aliases.zsh'
alias ef='e ~/functions.zsh'
alias el='e ~/variables.zsh'
alias ex='e ~/.Xresources'
alias ei='e ~/.config/i3/config'

# couple the s* aliases to the <leader>s* kbds in vim
alias sz='source ~/.zshrc'
alias sa='source ~/aliases.zsh'
alias sf='source ~/functions.zsh'
alias sl='source ~/variables.zsh'
alias sx='xrdb ~/.Xresources'
alias si='i3-msg restart'

# Google aliases
# blaze:      bz
# borgcfg:    br
# piper:      pi
# pastebin:   pb
# codesearch: cs
alias bzb='blaze build'
alias bzt='blaze test --test_output=all'
alias br='borgcfg'
alias pils='p4 listclients'
alias pirm='p4 citc -d'
alias pb=/google/src/head/depot/eng/tools/pastebin
alias pbc='p | pb --private --title $(date +${DATE_FMT})| tee >(c && chrome $(p))' # create a private gPaste from your clipboard's content; open the result in a browser
alias pbls='$BROWSER https://paste.googleplex.com/$(whoami)'
