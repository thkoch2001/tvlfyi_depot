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
#
# Most of this was ported from my aliases.zsh file, which I accumulated over a
# two to three year period. If some of the fish code herein is not idiomatic, it
# is most likely because I'm new to the ecosystem.

# TODO: Decide if I prefer `abbr` or `alias` for fish. `abbr` is a new concept
# for me.

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

# 64812159184761958540
source (fzf-share)/key-bindings.fish && fzf_key_bindings

# Miscellaneous
abbr --add c xclip -selection clipboard -i
abbr --add p xclip -selection clipboard -o
abbr --add cp cp_dwim
abbr --add lorem echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

# TODO: Ensure this works as expect with current EXWM setup.
abbr --add e emacsclient --no-wait --create-frame

abbr --add cat bat --theme=TwoDark
abbr --add j fasd_cd -d # Use j to emulate autojump; my muscle memory is hardened here
abbr --add vim nvim
abbr --add btctl bluetoothctl
abbr --add rg rg --ignore-case
abbr --add rgh rg --hidden # By default, rg skips hidden files
abbr --add fdh fd --hidden # By default, rg skips hidden files
abbr --add define sdcv # uses stardict to lookup a word
abbr --add intellij nohup /opt/intellij-ce-stable/bin/idea.sh >/dev/null 2>&1 &
abbr --add tpr tput reset
abbr --add nordvpn sudo openvpn /etc/openvpn/ovpn_tcp/us3559.nordvpn.com.tcp.ovpn # connects to the nordvpn servers in USA
abbr --add perms ls -ld # list the permissions of a directory
abbr --add rmrf rm -rf # sometimes the space and dash are too much...
abbr --add open xdg-open
abbr --add o open
abbr --add stopx sudo service lightdm stop # stop X server session
abbr --add please 'eval sudo $history[1]'
abbr --add plz please

# TODO: Consider packaging this with Nix.
abbr --add simple_vim vim -u ~/.config/nvim/simple.vim # vim without a zero-dependency vimrc

# Filesystem
abbr --add mdd mkdir_cd
abbr --add mdp mkdir --parents
abbr --add ls exa --sort=type
abbr --add ll exa --long --sort=type
abbr --add la exa --long --all --sort=type
abbr --add files laf
abbr --add dirs lad
abbr --add links lal

# Device and power management
abbr --add off shutdown now
abbr --add suspend systemctl suspend
abbr --add hibernate systemctl hibernate

abbr --add pscp pass show --clip

abbr --add wfls nmcli device wifi
abbr --add wfls nmcli device connect

# Tmux
abbr --add tls tmux list-sessions
abbr --add ta  tmux attach
abbr --add td  tmux detach

# Chrome
abbr --add chrome google-chrome
abbr --add cssh chrome --app-id=pnhechapfaindjhompbnflcldabbghjo # Secure Shell
abbr --add crd chrome --app-id=gbchcmhmhahfdphkhkmpfmihenigjmpp  # Chrome Remote Desktop

# Dropbox
abbr --add drst dropbox.py status

# Docker
abbr --add dk docker
abbr --add dkps docker ps
abbr --add dkpsa docker ps -a
abbr --add dkrm docker rm
abbr --add dkrmi docker rmi
abbr --add dkrd docker run -d
abbr --add dki docker images

# Java
# TODO: Consider packaging this idea with Nix instead
abbr --add jvsh env CLASSPATH=(fd '\\.jar$' ~/Dropbox/programming/jars | tr \\n :) jshell

# Elixir
abbr --add m mix
abbr --add mc mix compile
abbr --add mcf mix compile --force
abbr --add ism iex -S mix
abbr --add tism MIX_ENV=test iex -S mix
abbr --add mdg mix deps.get
abbr --add mdu mix deps.update
abbr --add mdup mix docker.up
abbr --add repl_ex dkish elixir iex

# Clojure
abbr --add cljsh dkish clojure lein repl

# GPG
abbr --add gpged gpg --edit-key wpcarro@gmail.com
abbr --add gpge gpg --encrypt
abbr --add gpgd gpg --decrypt
abbr --add gpgls gpg --list-keys

# Git
abbr --add glp   git log --graph --pretty='format:"%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset" --abbrev-commit --date=relative'
abbr --add g     hub
abbr --add git   hub
abbr --add ga    git add
abbr --add gc    git commit
abbr --add gp    git push
abbr --add grbi  git rebase --interactive
abbr --add grba  git rebase --abort
abbr --add grbc  git rebase --continue
abbr --add gprom git pull --rebase origin master
abbr --add gca   git commit --amend
abbr --add gcan  git commit --amend --no-edit
abbr --add gpf   git push --force
abbr --add gpff  git push --force --no-verify
abbr --add gds   git diff --staged
abbr --add gfx   git commit --fixup
abbr --add gsh   git show
abbr --add gwip  'git add . && git commit -m wip'
abbr --add gpr   git pull-request
abbr --add gst   'git status && hub pr list'

# Mercurial
# The attempt here is to map my well-known, existing `git` aliases to their
# Mercurial counterparts. Some may map 1:1, others may be like putting a square
# peg into a round hole. I will try and use my best judgement in these cases
# while erring on the side of unifying the two APIs.
abbr --add hgst PAGER="" hg status
abbr --add hglp hg xl
abbr --add hgp hg uploadchain # this is like `git push`
abbr --add hga hg add
abbr --add hgc hg commit
abbr --add hgcan hg amend # like `git commit --amend --no-edit'
abbr --add hgpr hg mail -r . -m # this may be similar to `hub pull-request`
abbr --add hgd hg diff
abbr --add hgsh hg export
abbr --add hgco hg update
abbr --add hgls hg citc --list # should have different output from `pils`
abbr --add hgrc hg rebase --continue
abbr --add hgra hg rebase --abort
abbr --add hgconflicts hg resolve --list 'set:unresolved()' # much like `gconflicts`
abbr --add hgrm hg citc -d # delete a CitC client created with Fig

# Haskell
abbr --add sb stack build
abbr --add se stack exec --
abbr --add sc stack clean
# alias st="stack test" # blocks suckless-terminal
abbr --add haddocks open (stack path --local-doc-root)/index.html
abbr --add hksh 'dkish haskell ghci'

# Kubernetes
abbr --add kc kubectl
abbr --add kpods kubectl get pods
abbr --add knodes kubectl get nodes
abbr --add kdeploys kubectl get deployments
abbr --add kdns kubectl get ing
abbr --add kedit kubectl edit deployments
abbr --add kswitch gcloud container clusters get-credentials

# Nix
abbr --add nq nix_introspect
abbr --add nsh nix-shell
abbr --add nshp nix-shell --pure
abbr --add nr nix repl
abbr --add ni nix-env --install
abbr --add nrm nix-env --uninstall
abbr --add nls nix-env --query
abbr --add nrs sudo nixos-rebuild switch

# Aptitude (apt)
abbr --add apti sudo apt-get install --assume-yes
abbr --add aptrm sudo apt remove

# Pacman
abbr --add pmi sudo pacman -S --noconfirm
abbr --add pms pacman -Ss
abbr --add pmrm sudo pacman -Rs

# Couple the e* aliases to the <leader>e* kbds in vim.
abbr --add ev e ~/.config/nvim/init.vim
abbr --add ee e ~/.emacs.d/init.el
abbr --add ez e ~/.zshrc
abbr --add ea e ~/aliases.zsh
abbr --add ef e ~/functions.zsh
abbr --add el e ~/variables.zsh
abbr --add ex e ~/.Xresources
abbr --add ei e ~/.config/i3/config
abbr --add em e ~/.tmux.conf
abbr --add er e ~/Dropbox/dotfiles/README.md
# TODO: consider DRYing this up with `e`. Unfortunately, `sudo` won't support
# aliases.
abbr --add en sudo ALTERNATE_EDITOR=nvim emacsclient --no-wait --create-frame /etc/nixos/configuration.nix

# Couple the s* aliases to the <leader>s* kbds in vim.
abbr --add sz source ~/.zshrc
abbr --add sa source ~/aliases.zsh
abbr --add sf source ~/functions.zsh
abbr --add sl source ~/variables.zsh
abbr --add sx xrdb ~/.Xresources
abbr --add si i3-msg restart
abbr --add sm tmux source-file ~/.tmux.conf
abbr --add sn sudo nixos-rebuild switch

# CircleCI
abbr --add cci    circleci local
abbr --add ccijob circleci local execute --job

# Google stuff
abbr --add bzb      blaze build
abbr --add bzt      blaze test --test_output=all
abbr --add br       borgcfg
abbr --add pils     p4 listclients
abbr --add pirm     p4 citc -d
abbr --add ppls     'g4 listclients | sed \'s/^Client wpcarro://\' | sed \'s/:[0-9]*:citc.*$//g\''
abbr --add pprm     p4 citc -d -f # WARNING: This will forcefully delete a CitC client even if contains pending changes.
abbr --add flagpick /google/data/ro/users/sk/skaushik/www/public-tools/flagpick
abbr --add jaze     /google/data/ro/projects/devtools/javascript/jaze
abbr --add aclcheck /google/data/ro/projects/ganpati/aclcheck
abbr --add g3python /google/data/ro/projects/g3python/g3python
abbr --add pb       /google/src/head/depot/eng/tools/pastebin
abbr --add pbc      'pb --private --title $(date +$date_fmt)| tee >(c && chrome (p))' # create a private gPaste from your clipboard's content; open the result in a browser
abbr --add pbcp     'p | pb --private --title (date +$date_fmt)| tee >(c && chrome (p))' # create a private gPaste from your clipboard's content; open the result in a browser
abbr --add pbls     $BROWSER https://paste.googleplex.com/(whoami)
