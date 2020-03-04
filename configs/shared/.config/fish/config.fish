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

# Adding this to support tramp.
if test "$TERM" = "dumb"
    function fish_prompt
        echo "\$ "
    end
    function fish_right_prompt; end
    function fish_greeting; end
    function fish_title; end
else
    # Remove the default greeting from fish
    set fish_greeting ""

    # Prompt
    function fish_prompt
        # My custom prompt.
        #
        # Design objectives:
        # - max-length <= 80 characters
        # - minimal
        # - no dependencies (well, you know what I mean)
        #
        # Components
        # - ssh connection
        # - user
        # - host
        # - git repo
        # - git branch
        # - lambda character as prompt

        # Cache status before we overwrite it.
        set -l last_status $status

        # Colors
        set -l color_inactive (set_color red --bold)
        set -l color_active (set_color green --bold)
        set -l color_normal (set_color normal)

        # SSH information
        if set -q SSH_CLIENT; or set -q SSH_TTY
            echo -en "$color_active \bssh ✓ [$color_normal$USER@"(hostname)"$color_active]$color_normal"
        else
            echo -en "$color_inactive \bssh ✗ [$color_normal$USER@"(hostname)"$color_inactive]$color_normal"
        end

        # Separator
        echo -n " "

        # Git information
        set -l git_repo (git rev-parse --show-toplevel 2>/dev/null)
        set -l git_status $status
        set -l dir_parent (basename (realpath ..))
        set -l dir_current (basename (realpath .))
        if test $git_status -eq 0
            set -l git_repo_name (basename (git rev-parse --show-toplevel))
            set -l git_branch (git branch 2>/dev/null | grep '^\*' | cut -d' ' -f2-)
            echo -en "$color_active \bgit ✓ [$color_normal$git_branch$color_active|$color_normal$git_repo_name$color_active|$color_normal$dir_parent/$dir_current$color_active]$color_normal"
        else
            echo -en "$color_inactive \bgit ✗ [$color_normal$dir_parent/$dir_current$color_inactive]$color_normal"
        end

        # Newline
        echo

        # Handle root vs non-root
        if [ "$USER" = "root" ]
            set -g prompt_sigil "#"
        else
            set -g prompt_sigil "λ"
        end

        # TODO(wpcarro): For root directories like /tmp, there will not be a parent
        # directory. Support these directories.
        set -l time (date +"%T")
        if test $last_status -eq 0
            # TODO(wpcarro): I'd prefer to use black here instead of white, but for
            # some reason white is black and black is invisible.
            set -l color_prompt (set_color white --bold)
            echo -n "$time$color_prompt $prompt_sigil$color_normal "
        else
            set -l color_prompt (set_color red --bold)
            echo -n "$time$color_prompt $prompt_sigil$color_normal "
        end
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
end
