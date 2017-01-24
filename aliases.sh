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

# view the Unix directory stack as maintained through
#     pushd and popd commands
alias wdirs='dirs | tr " " "\n" | sort -r'

