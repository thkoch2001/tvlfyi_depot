alias c="clear"
alias dir='find . -maxdepth 1 -type d -regex "\.\/[^.].+"'


# aliases with dependencies
command -v nvim >/dev/null && alias vim=nvim || \
        echo 'Missing dependency (nvim). Failed to alias vim -> nvim'
command -v find >/dev/null && alias find='find -E' || \
        echo 'Missing dependency (find -E). Failed to alias find -> find -E'
command -v egrep >/dev/null && alias grep=egrep || \
        echo 'Missing dependency (egrep). Failed to alias grep -> egrep'
command -v pygmentize >/dev/null && alias ccat='pygmentize -g' >/dev/null || \
        echo 'Missing dependency (pygmentize -g). Failed to alias ccat -> pygmentize -g'
command -v hub >/dev/null && alias git=hub || \
        echo 'Missing dependency (hub). Failed to alias git -> hub'
command -v tmux >/dev/null && alias tls='tmux list-sessions' || \
        echo 'Missing dependency (tmux). Failed to alias tls -> tmux list-sessions'
command -v gpg2 >/dev/null && alias gpg=gpg2 || \
        echo 'Missing dependency (gpg2). Failed to alias gpg -> gpg2'


# exa-specific aliases
if command -v exa >/dev/null || echo 'Missing dependency (exa). Failed to create dependent aliases.'; then
  alias ls='exa'
  alias ll='exa -l'
  alias ll='ls -l'
  alias la='exa -la'
  alias la='ls -la'

  # tree-views of dirs and subdirs
  function lt {
    if [ -z $1 ]; then
      exa --tree
    else
      depth=$1
      exa --tree --level ${depth}
    fi
  }
fi


if command -v kubectl >/dev/null; then
  # kubernetes aliases
  alias kpods='kubectl get pods'
  alias kdeploys='kubectl get deployments'

  # the following functions rely on gcloud being installed
  if command -v gcloud >/dev/null; then
    function kswitch {
      # environment should be "staging" or "public"
      environment=$1
      gcloud container clusters get-credentials $environment
    }
  fi

  # kubernetes functions
  function kedit {
    deployment=$1
    kubectl edit deployments $deployment
  }

  function kdns {
    # Returns the domain names of the ingress points
    kubectl get ing
  }

  function kush {
    name=$1
    cmd=${2-/bin/bash}
    kubectl exec -it $name -- $cmd
  }
fi


# git-specific aliases
git config --global alias.recent 'for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)"'
git config --global alias.yday '! git log --name-only --since=yesterday.midnight --until=today.midnight --author="$(git config --get user.email)"'
git config --global alias.conflicts 'diff --name-only --diff-filter=U'
git config --global alias.patch-grep 'log -p -S'

alias gyday='git log --name-only --since=yesterday.midnight --until=today.midnight --author="$(git config --get user.email)"'

alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gprom="git pull --rebase origin master"
alias gcan='git commit --amend --no-edit'
alias gpf='git push --force'
alias gpff='git push --force --no-verify' # aka git push fucking force
alias gds='git diff --staged'
alias gfx='git commit --fixup'
alias gsh='git show'


# elixir-specific aliases
alias m=mix
alias ism='iex -S mix'
alias tism='MIX_ENV=test iex -S mix'
alias mdg='mix deps.get'


# docker-specific aliases
if command -v docker >/dev/null; then
  # personal aliases
  alias d=docker
  alias dps='docker ps'
  alias drm='docker rm'
  alias drmi='docker rmi'
  alias drit='docker run -it'
  alias drd='docker run -d'
  alias di='docker images'

  function dsh {
    container=$1
    docker exec -it "${container}" /bin/bash
  }
fi
