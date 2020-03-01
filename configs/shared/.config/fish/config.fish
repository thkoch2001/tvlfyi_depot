# While I work use a variety of programs, below of some of my more commonly used
# programs that I have decided to support with aliases.
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
# Below are some common modifiers or flags that programs support.
# Supported qualifiers:
#   hidden:      h
#   ignore-case: i
#
# I've found that much of my time is spent working with programs that support
# some many of the following actions.
# Supported verbs:
#   source:  s
#   install: i
#   test:    t
#   build:   b
#   list:    ls
#   shell:   REPL
#
# Commentary:
# Ideally a file like this would be either unnecessary in the case of a fully
# embraced Emacs workflow or compiled from some high-level language like
# Elisp.

# Remove the default greeting from fish
set fish_greeting ""

# Prompt
function fish_prompt
    set -l color_cwd
    set -l suffix
    switch "$USER"
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
            set suffix '#'
        case '*'
            set color_cwd $fish_color_cwd
            set suffix '>'
    end

    echo -n -s "$USER" @ (prompt_hostname) ' ' (set_color $color_cwd) (pwd) (set_color normal)
    echo -e "\n$suffix "
end

source ./functions.fish

# Setup fzf for fuzzily finding commands, files, directories
source (fzf-share)/key-bindings.fish && fzf_key_bindings

# TODO: What is the difference between `source` and `eval`
# direnv
eval (direnv hook fish)

# Miscellaneous
alias c='xclip -selection clipboard -i'
alias p='xclip -selection clipboard -o'
alias lorem='echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."'
alias cat='bat --theme=TwoDark'
alias vim=nvim
alias rg='rg --ignore-case'
alias rgh='rg --hidden' # By default, rg skips hidden files
alias fdh='fd --hidden' # By default, fd skips hidden files
alias tpr='tput reset'
alias define=sdcv # uses stardict to lookup a word
alias perms='ls -ld' # list the permissions of a directory
alias rmrf='rm -rf' # sometimes the space and dash are too much...
alias open=xdg-open
alias stopx='sudo service lightdm stop' # stop X server session
alias please='eval sudo $history[1]'
alias chrome=google-chrome
alias sys='systemctl'
alias sysu='systemctl --user'

# Filesystem
# TODO: Depend on `mkdir_cd`.
alias mdd=mkdir_cd
alias ls='exa --sort=type'
alias ll='exa --long --sort=type'
alias la='exa --long --all --sort=type'
# TODO: Depend on these functions once they're defined.
alias files=laf
alias dirs=lad
alias links=lal

# Device and power management
alias off='shutdown now'
alias suspend='systemctl suspend'
alias hibernate='systemctl hibernate'

# TODO: Debug `Error: No interface specified.`.
alias wfls='nmcli device wifi'
alias wfc='nmcli device connect'

# Tmux
alias tls='tmux list-sessions'
alias ta ='tmux attach'
alias td ='tmux detach'

# Dropbox
alias drst='dropbox.py status'

# GPG
alias gpged='gpg --edit-key wpcarro@gmail.com'
alias gpge='gpg --encrypt'
alias gpgd='gpg --decrypt'
alias gpgls='gpg --list-keys'

# Git
# alias glp='git log --graph --pretty="format:"%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset" --abbrev-commit --date=relative'
alias g=hub
alias git=hub
alias ga='git add'
alias gc='git commit'
alias gco='git checkout'
alias gd='git diff'
alias gp='git push'
alias grbi='git rebase --interactive'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias gprom='git pull --rebase origin master'
alias gca='git commit --amend'
alias gcan='git commit --amend --no-edit'
alias gpf='git push --force'
alias gpff='git push --force --no-verify'
alias gds='git diff --staged'
alias gfx='git commit --fixup'
alias gsh='git show'
alias gwip='git add . && git commit -m wip'
alias gpr='git pull-request'
alias gst='git status && hub pr list'

# Mercurial
# The attempt here is to map my well-known, existing `git` aliases to their
# Mercurial counterparts. Some may map 1:1, others may be like putting a square
# peg into a round hole. I will try and use my best judgement in these cases
# while erring on the side of unifying the two APIs.
alias hgst='hg status'
alias hglp'=hg xl'
alias hgp='hg uploadchain'
alias hga='hg add'
alias hgc='hg commit'
alias hgcan='hg amend'
alias hgpr='hg mail -r . -m'
alias hgd='hg diff'
alias hgsh='hg export'
alias hgco='hg update'
alias hgls='hg citc --list'
alias hgrc='hg rebase --continue'
alias hgra='hg rebase --abort'
alias hgrm='hg citc -d'
alias hgconflicts='hg resolve --list "set:unresolved()"'

# Aptitude (apt)
alias apti='sudo apt-get install --assume-yes'
alias aptrm='sudo apt remove'

# Google stuff
alias bzb='blaze build'
alias bzt='blaze test --test_output=all'
alias br=borgcfg
alias pils='p4 listclients'
alias pirm='p4 citc -d'
alias ppls='g4 listclients | sed \'s/^Client wpcarro://\' | sed \'s/:[0-9]*:citc.*$//g\''
alias pprm='p4 citc -d -f' # WARNING: This will forcefully delete a CitC client even if contains pending changes.
alias aclcheck=/google/data/ro/projects/ganpati/aclcheck
