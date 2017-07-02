alias ll="ls -l"
alias c="clear"
alias dir='find . -maxdepth 1 -type d -regex "\.\/[^.].+"'


# aliases with dependencies
command -v nvim >/dev/null && alias vim=nvim || \
        echo "Missing dependency (nvim). Failed to alias vim -> nvim"
command -v find >/dev/null && alias find='find -E' || \
        echo "Missing dependency (find -E). Failed to alias find -> find -E"
command -v egrep >/dev/null && alias grep=egrep || \
        echo "Missing dependency (egrep). Failed to alias grep -> egrep"
command -v pygmentize >/dev/null && alias ccat='pygmentize -g' >/dev/null || \
        echo "Missing dependency (pygmentize -g). Failed to alias ccat -> pygmentize -g"
command -v hub >/dev/null && alias git=hub || \
        echo "Missing dependency (hub). Failed to alias git -> hub"
command -v tmux >/dev/null && alias tls='tmux list-sessions' || \
        echo "Missing dependency (tmux). Failed to alias tls -> tmux list-sessions"


# git-specific aliases
git config --global alias.recent 'for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)"'
git config --global alias.today 'log --since=00:00:00 --all --no-merges --oneline --author="$(git config --get user.email)"'

alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gprom="git pull --rebase origin master"
alias gcan='git commit --amend --no-edit'
alias gpf='git push --force'
alias gds='git diff --staged'
