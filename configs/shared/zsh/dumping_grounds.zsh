#!/usr/bin/env zsh

# Docker
dsh() {
  # Drop into a Docker shell. Shell defaults to /bin/bash.
  container=$1
  cmd=${2-/bin/bash}

  docker exec -it "${container}" "${cmd}"
}

# Emacs
dired() {
  # Opens either the `$(pwd)` or `$1` in Emacs's `dired`.
  # Uses i3 to focus Emacs.
  directory=${1:-$(pwd)}
  echo $directory
  emacsclient --eval "(dired \"$directory\")" && focus Emacs
}

org_capture() {
  # Spawns an Emacs frame running org-capture.
  echo called
  emacsclient --create-frame \
              --frame-parameters '(quote (name . "org-protocol-capture"))' \
              --eval '(org-capture)'
}

# Git
conflicts() {
  # Edit git conflicts one-by-one in your favorite editor.
  ${EDITOR} "$(git status --porcelain | awk '/^UU/ { print $2 }')"
}

# GPG
gpg_encrypt() {
  # Convenience function around encryping files and directories.
  # Appends a .gpg extension and deletes the unencrypted source.
  local file=${1}

  echo "Encrypting..."

  if [ -f "${file}" ]; then
    gpg --symmetric "${file}" && \
    rm "${file}"

  elif [ -d "${file}" ]; then
    tar -cz "${file}" | gpg --symmetric --output "${file}.tar.gz.gpg"
  fi

  echo "Done."
}

gpg_decrypt() {
  # Convenience function around decrypting .gpg files and directories.
  # Deletes the original encrypted file with the .gpg extension.
  local file=$1

  echo "Decrypting..."

  if [ -f "${file}" ]; then
    gpg --decrypt "${file}" >"${file%.gpg}" && \
    rm "${file}"

  elif [ -d "${file}" ]; then
    local outdir="${dirname%.tar.gz.gpg}"

    if [ -d "${outdir}" ]; then
      echo "Output directory, ${outdir}, already exists and will be overwritten by this command. Aborting..."
      return 1
    else
      gpg --decrypt "${dirname}" | tar -xv
    fi
  fi

  echo "Done."
}

# Python
python_sandbox() {
  # Creates a nix-shell with the specified arguments as Python packages
  nix-shell -p "python36.withPackages(p: with p; [$@])"
}

# Haskell
cabal_unhell() {
  # Run this function to save yourself from Cabal hell.
  # Note: this will require that you reinstall packages for your projects again.
  find ~/.ghc -maxdepth 1 -type d -exec rm -rf {} \;
  rm -rf ~/.cabal/{lib,packages,share}
}

haskell_sandbox() {
  # Creates a nix-shell with the specified arguments as Haskell packages
  nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [$@])"
}

_haskell_test_watch_path() {
  # Runs and watches the tests for a provided file path.
  ghcid -c "stack ghci grid:lib grid:grid-test --ghci-options=-fobject-code" \
    --height="$(tput lines)" --width="$(tput cols)" --warnings --test "$1"
}

_haskell_test_watch_pattern() {
  # Runs and watches the tests that match a provided pattern.
  stack test --file-watch grid:grid-test --ta "-p \"${1}\""
}

haskell_test_watch() {
  # Accepts either a filepath or a pattern and runs a test-watcher for either.
  if [ -f "$1" ]; then
    _haskell_test_watch_path "$1"
  else
    _haskell_test_watch_pattern "$1"
  fi
}

# Kubernetes
kush() {
  # Drop into a shell via Kubernetes. Shell defaults to /bin/bash.
  local name=$1
  local cmd=${2-/bin/bash}

  kubectl exec -it "${name}" -- "${cmd}"
}

# Misc
all_users() {
  # Lists all of the known users in the Linux system
  # Useful because when you type `~art` in a prompt and tab-complete, ZSH looks
  # up all users whose names start with "art". It's also just interesting to
  # have access to this information.
  #
  # NOTE: this is not as simple as `cat /etc/passwd` for reasons of which I'm
  # not entirely sure.
  getent passwd
}

test_true_color() {
  # Run this to test if your terminal emulator supports True Color
  curl --silent https://raw.githubusercontent.com/JohnMorales/dotfiles/master/colors/24-bit-color.sh | bash
}

path() {
  # Pretty-print the $PATH variable
  echo "$PATH" | tr : '\n'
}

nix_installed() {
  # Lists the packages installed with `nix-env`
  nix-env -q
}

nix_store() {
  # Print the packages in /nix/store without the preceding hash
  ls /nix/store | sed 's/[a-z0-9]*-//'
}

browse() {
  # Open a URL in $BROWSER. Friendly for terminal input and output.
  nohup "$BROWSER" $@ &
}

essids() {
  # Returns a list of all ESSIDs the network card detects
  local interface=${1-wlp4s0}
  sudo iwlist "${interface}" scan | awk -F \" '{print $2}' | sed '/^\s*$/d'
}

mkdir_cd() {
  # Make and cd into a directory or path
  mkdir -p "$1" && cd "$1"
}

swap() {
  # Swaps the names of files and directories.
  local file_a="${1}"
  local file_b="${2}"

  if [ -d "${file_a}" ] && [ -d "${file_b}" ]; then
    local backup=$(mktemp -d backup.XXX)

    mv "${file_a}" "${backup}"
    mv "${file_b}" "${file_a}"
    mv "${backup}/${file_a}" "${file_b}"
    rm -rf "${backup}"
  elif [ -f "${file_a}" ] && [ -f "${file_b}" ]; then
    local backup=$(mktemp backup.XXX)

    mv "${file_a}" "${backup}"
    mv "${file_b}" "${file_a}"
    mv "${backup}" "${file_b}"
    rm "${backup}"
  fi

  echo "Swapped: ${file_a} <-> ${file_b}"
}

bak() {
  # Backup a file or a directory by appending a .bak extension to it.
  mv "$1" "$1.bak"
}

unbak() {
  # Restore a file by removing the .bak extension from it.
  mv "$1.bak" "$1"
}

is_online() {
  # Pings google.com and echos "Online" or "Offline" and returns the appropriate
  # exit code. Could be useful in the ${PS1} variable.
  wget -q --spider "http://google.com"

  if [ $? -eq 0 ]; then
    echo "Online"
    return 0
  else
    echo "Offline"
    return 1
  fi
}

du_it_live() {
  # Outputs and refreshes the size of a directory's content.
  # Useful for watching a directory as large amounts of data are
  # downloaded into it.
  local directory="${1}"

  while true; do
    du -hc "${directory}" | tail -n 1 | tr -d '\n' && echo -n ' ' && sleep 0.5

    # elipsis
    echo -n '.' && sleep 0.5 &&
    echo -n '.' && sleep 0.5 &&
    echo -n '.' && sleep 0.5 &&

    # clear the three-dots
    echo -n '\b\b\b' && echo -n '   ' && echo -n '\r'
  done
}

router() {
  # Returns the IP address of the network's router.
  # Useful in a call like `ping $(router)` to diagnose an internet problem.
  netstat -nr | grep default | head -n 1 | awk '{ print $2 }'
}

monitor_dimensions() {
  # Outputs the dimensions of your computer monitor
  xdpyinfo | awk '/dimensions/{ print $2 }'
}

list_sinks() {
  # Lists the available output sources (speakers?)
  pacmd list-sinks | grep -e 'name:' -e 'index:'
}

list_sources() {
  # List available input sources (microphones?)
  pacmd list-sources | grep -e 'index:' -e device.string -e 'name:'
}

lt() {
  # Convenience wrapper around `exa --tree`.
  # Optionally accepts a number for the max-depth and a directory to list.
  # $ lt 2 ./scripts

  # lt
  if [ -z ${1} ]; then
    exa --tree --all

  # lt 2
  elif [[ "${1}" =~ '^[0-9]+$' ]] && [ -z ${2} ]; then
    local depth="${1}"

    exa --tree -all --level "${depth}"

  # lt ./scripts
  elif [ -z ${2} ]; then
    local directory="${1}"

    exa --tree --all "${directory}"

  # lt 2 ./scripts
  else
    local depth=${1}
    local directory="${2}"

    exa --tree --all --level ${depth} "${directory}"
  fi
}

gql() {
  # Convenience wrapper around `http POST` that allows you write GQL queries in
  # Vim before posting them to the server.
  local endpoint="${1}"
  local query="/tmp/http-query.gql"

  vim "${query}" && \
  echo "{\"query\":\"$(cat ${query})\"}" | \
  http --body POST "${endpoint}"
}

# Nix
nix_introspect() {
  # Greps through my local nixpkgs repo for
  rg --after-context 5 "\\b$1\\b\\s*=" "$(nix-instantiate --find-file nixpkgs)"
}

# Tmux
t() {
  # Find or create a Tmux session.
  local session_name="${1}"
  if ! tmux has-session -t "${session_name}" 2> /dev/null; then
    local oldTMUX="${TMUX}"
    unset TMUX
    tmux new -d -s "${session_name}" -n "${session_name}"
    export TMUX="${oldTMUX}"
    unset oldTMUX

    if command -v j >/dev/null; then
      tmux send-keys -t "${session_name}" "j ${session_name}; clear" "C-m"
    else
      tmux send-keys -t "${session_name}"
    fi
  fi
  if [[ -n "${TMUX}" ]]; then
    tmux switch-client -t "${session_name}"
  else
    tmux attach -t "${session_name}"
  fi
}

tk() {
  # `tk`: "tmux kill". Kills a tmux session by name.
  # If no arguments are provided, kills the current session after jumping to the previous session.
  session_name="${1}"
  if [ ! -z "${session_name}" ]; then
    tmux kill-session -t "${session_name}"
  else
    session_name=tmux ls -f '#{?session_attached,#{session_name},}' | xargs
    tmux switch-client -l
    tmux kill-session -t "${session_name}"
  fi
}

tmux_is_running() {
  # Returns zero if tmux is running
  # Although this is a simple function body, it's useful to encode esoteric
  # knowledge that I will easily forget.
  test -n "$TMUX"
}

tmux_focused_pane() {
  # Returns the ID of the focused tmux pane.
  # WIP
  # tmux list-panes -F '#{pane_active} #{pane_tty}' | awk /1/{ print $1 }
  echo 'Not implemented'
}

# zsh
fns() {
  # Outputs all available functions.
  # `fns` was chosen instead of `functions`, since `functions` was already
  # taken.
  compgen -A function
}

aliases() {
  # Outputs all available aliases.
  compgen -a
}

keywords() {
  # Outputs all of the shell's reserved keywords.
  compgen -k
}

builtins() {
  # Outputs all of the shell's builtin commands.
  compgen -b
}

zle_insert_subshell() {
  LBUFFER+='$(' ; RBUFFER=")$RBUFFER"
}
zle -N zle_insert_subshell
bindkey '^j' zle_insert_subshell

zle_insert_variable() {
  LBUFFER+='${' ; RBUFFER="}$RBUFFER"
}
zle -N zle_insert_variable
bindkey '^v' zle_insert_variable

zle_insert_2x_dash() {
  LBUFFER+=' --'
}
zle -N zle_insert_2x_dash
bindkey '^[^f' zle_insert_2x_dash

zle_insert_2x_quote() {
  LBUFFER+=' "' ; RBUFFER="\"$RBUFFER"
}
zle -N zle_insert_2x_quote
bindkey '^["' zle_insert_2x_quote

zle_insert_quote() {
  LBUFFER+=" '" ; RBUFFER="'$RBUFFER"
}
zle -N zle_insert_quote
bindkey "^['" zle_insert_quote
