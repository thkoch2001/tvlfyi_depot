# These were haphazardly ported from wpcarro/nixify, so some may be broken.
alias c="xclip -selection clipboard -i"
alias p="xclip -selection clipboard -o"
alias md="mkdir_cd"
alias ls="exa"
alias ll="exa -l"
alias la="exa -la"
alias lorem="echo Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
alias e="emacsclient --no-wait"
alias d="docker"
alias dps="docker ps"
alias dpsa="docker ps -a"
alias drm="docker rm"
alias drmi="docker rmi"
alias drit="docker run -it"
alias drd="docker run -d"
alias di="docker images"
alias m="mix"
alias mc="mix compile"
alias mcf="mix compile --force"
alias ism="iex -S mix"
alias tism="MIX_ENV=test iex -S mix"
alias mdg="mix deps.get"
alias mdu="mix deps.update"
alias mdup="mix docker.up"
alias cat="bat"
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
alias sb="stack build"
alias se="stack exec --"
alias sc="stack clean"
alias st="stack test"
# Currently broken
# alias haddocks=''open "$(stack path --local-doc-root)/index.html"''
alias kc="kubectl"
alias kpods="kubectl get pods"
alias knodes="kubectl get nodes"
alias kdeploys="kubectl get deployments"
alias kdns="kubectl get ing"
alias kedit="kubectl edit deployments "
alias kswitch="gcloud container clusters get-credentials "
alias nq="nix_introspect"
alias nsh="nix-shell"
alias nshp="nix-shell --pure"
alias nr="nix repl"
alias md=mkdir_cd
alias j='fasd_cd -d' # to emulate autojump; my muscle memory is hardened here
alias vim=nvim # prefer neovim to vim
alias links='find ~ -maxdepth 1 -type l -exec exa {} \;' # list all of the links in the home directory
alias ra=ranger
alias chrome=google-chrome
alias btctl=bluetoothctl
alias rg='rg --hidden' # By default, rg skips hidden files
alias fd='fd --hidden' # By default, rg skips hidden files

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
# blaze:    bz
# borgcfg:  bg (NOTE: `bg` already exists as a shell built-in)
# piper:    pi
# pastebin: pb
alias bzb='blaze build'
alias bg='borgcfg'
alias pils='p4 listclients'
alias pirm='p4 citc -d'
alias pb=/google/src/head/depot/eng/tools/pastebin
alias pbc='p | pb --private --title $(date +${DATE_FMT})| tee >(c && chrome $(p))' # create a private gPaste from your clipboard's content; open the result in a browser
alias pbls='$BROWSER https://paste.googleplex.com/$(whoami)'
