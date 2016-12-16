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

# view the Unix directory stack as maintained through
#     pushd and popd commands
alias wdirs='dirs | tr " " "\n" | sort -r'

# Pretty-print git history
alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

