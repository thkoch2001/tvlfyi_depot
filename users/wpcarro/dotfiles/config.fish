alias c 'xclip -selection clipboard -i'
alias p 'xclip -selection clipboard -o'
alias cat 'bat --theme="Monokai Extended Light"'
alias rgh 'rg --hidden'
alias fdh 'fd --hidden'
alias tpr 'tput reset'
alias ls 'exa --sort=type'
alias ll 'exa --long --sort=type'
alias la 'exa --long --all --sort=type'
alias gcan 'git commit --amend --no-edit'
alias gd 'git diff'
alias gds 'git diff --staged'
alias glp 'git log --pretty --oneline --graph'
alias gpf 'git push --force-with-lease'
alias gsh 'git show HEAD'
alias gst 'git status'
alias edit 'emacsclient -n'
# fs navigation
alias d 'cd /depot'
alias w 'cd /depot/users/wpcarro'
alias sc 'systemctl'

# environment variables
set -gx EDITOR "emacsclient"
set -gx ALTERNATE_EDITOR "emacs -q -nw"
set -gx VISUAL "emacsclient"

# Use my custom fish prompt
source /depot/users/wpcarro/dotfiles/prompt.fish

# Configure fuzzy history, file, directory searching
source (fzf-share)/key-bindings.fish && fzf_key_bindings

# Install direnv
eval (direnv hook fish)
