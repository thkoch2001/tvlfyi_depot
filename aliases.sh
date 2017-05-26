alias ll="ls -l"

alias catn="cat -n"

alias egrep="egrep --color"

alias grep="egrep"

alias c="clear"

alias vim="nvim"

alias find="find -E"

alias dir='find . -maxdepth 1 -type d -regex "\.\/[^.].+"'

# if pygmentize isn't installed run...
# sudo pip install pygments
# colorizes syntax of files
alias ccat="pygmentize -g"

# self-evident git alias
alias gprom="git pull --rebase origin master"

alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

# view the Unix directory stack as maintained through
#     pushd and popd commands
alias wdirs='dirs | tr " " "\n" | sort -r'

# GitHub integration
alias git=hub

# Git aliases
# List MRU branches
git config --global alias.recent 'for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)"'

# List today's work
git config --global alias.today 'log --since=00:00:00 --all --no-merges --oneline --author="$(git config --get user.email)"'

# git commit --amend --no-edit
alias gcan='git commit --amend --no-edit'

# git commit --amend --no-edit
alias gpf='git push --force'
