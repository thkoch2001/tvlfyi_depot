prodaccess() {
  # Wraps existing `prodaccess` command to provide Google-specific tips.
  # Take from this: https://g3doc.corp.google.com/experimental/users/diamondm/fortunes/README.md?cl=head
  command prodaccess "$@" && \
    cowsay $(/google/data/ro/users/di/diamondm/engfortunes/fortune.sh --extra_space)
}

fv() {
  # Usage: fv file pattern
  # This is useful when you know the fuzzy name of the file you want to edit
  local file
  file="$(fzf --exact --height 40% --reverse --query="$1"  --select-1 --exit-0)"
  [[ -n "$file" ]] && vim "$file"
}

tbz() {
  # Toggle between blaze-bin and your source.
  # Useful if you like to cd into the dir where your source lives.
  if [[ "$(pwd)" =~ '(.*)/blaze-bin(.*)' ]]; then
    cd "${match[1]}${match[2]}"
  else
    cd "${PWD/\/google3//google3/blaze-bin}"
  fi
}

tj() {
  # Toggle between the source dir and test dir in a Java project.
  if [[ $PWD =~ '(.*)/javatests(.*)' ]]; then
    cd "${match[1]}/java${match[2]}"
  else
    cd "${PWD/\/google3\/java//google3/javatests}"
  fi
}

snipit() {
  # Take a screenshot and host it at https://screenshot.googleplex.com
  # Adapted from SnipIt to fit my workflow.
  server="https://screenshot.googleplex.com/upload"
  file="${TEMP:-/tmp}/snipit_temp_$$.png"

  # Capture
  echo "SnipIt - Click a window, or drag to snip a region (Ctrl+C to cancel):" && \
    import "$file" && \
    echo "Sending image to server..." && \
    uri=$(curl -sF "imagedata=@$file" $server) && \
    c <<<"$uri" && \
    echo "Copied \"$uri\" to your clipboard."
}

# Java
run_java() {
  # Usage: run_java path/to/file.java
  # Intended to be similar in spirit to Haskell's `runhaskell`.
  set -e
  javac $1
  java ${1%.java}
}

# Aptitude
apts() {
  # Searches aptitude package repository for $1, surrounding it in ^$ to ensure
  # fewer results.
  apt search "^$1$"
}

# Docker
dkcsh() {
  # Drop into a Docker shell. Shell defaults to /bin/bash.
  # `dkcsh` stands for DocKer Container SHell.
  # Usage: dkcsh
  container=$1
  cmd=${2-/bin/bash}

  docker exec -it "$container" "$cmd"
}

dkish() {
  # Runs a Docker container with `/usr/bin/env bash`.
  # `dkish` stands for DocKer Image SHell.
  # Note: This defers from `dksh`, which accepts a container instead of an
  # image.
  # Usage: dkrit <container_name> [command]
  image=$1
  cmd=${2-/bin/bash}

  docker run -it "$image" "$cmd"
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
update_x11_forwarding() {
  # Sometime Tmux misbehaves with X11 applications (e.g. Emacs). This is because
  # the DISPLAY variable is not set properly to `:0`. This function w
  # Cache the DISPLAY when outside of Tmux. When inside of Tmux, use the cached
  # value for DISPLAY.
  #
  # This cooperates with my `preexec` function, which runs before every command.
  # Adapted from here: http://alexteichman.com/octo/blog/2014/01/01/x11-forwarding-and-terminal-multiplexers/
  if [ -z "$TMUX" ]; then
    echo $DISPLAY > ~/.display.txt
  else
    export DISPLAY=$(cat ~/.display.txt)
  fi
}

monzo_balance() {
  # Return the balance of my Monzo bank account. Intended to be used in my i3
  # status bar.
  # Usage: monzo_balance
  # Depends:
  #   - ~/Dropbox/monzo_creds.json.gpg (encrypted asymmetrically for yourself)
  #   - httpie
  #   - jq
  #   - gpg
  local creds=$(gpg --decrypt ~/Dropbox/monzo_creds.json.gpg 2>/dev/null)
  local access_token=$(echo $creds | jq --raw-output .access_token)
  local account_id=$(echo $creds | jq --raw-output .account_id)
  local balance=$(http --body https://api.monzo.com/balance \
                       "Authorization: Bearer ${access_token}" \
                       "account_id==${account_id}" | \
                    jq .balance)

  echo "£$balance"
}

tldr_docs() {
  # Helper function for submitting a new page to `tldr`.
  # Usage: tldr_docs <cmd-name>
  pushd ~/programming/tldr && \
    gcb "$1" && \
    "$EDITOR" . && \
    echo "Next steps:" && \
    echo "- commit changes" && \
    echo "- push changes" && \
    echo "- submit a pull-request to tldr" && \
    popd # return to the original directory
}

ord_to_char() {
  # Converts the ordinal, ASCII value of a character into its encoded
  # representation.
  #
  # Usage:
  # $ ord_to_char 65
  # A
  [ "$1" -lt 256 ] || return 1
  printf "\\$(printf '%03o' "$1")\n"
}

char_to_ord() {
  # Converts the ASCII representation of a character to its ordinal value.
  #
  # Usage:
  # $ char_to_ord A
  # 65
  LC_CTYPE=C printf '%d\n' "'$1"
}

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

escape_sequences() {
  # Outputs a table of terminal escape sequences and their meaning.
  echo -E '\a         Bell (alert)'
  echo -E '\b         Backspace'
  echo -E '\f         Formfeed'
  echo -E '\n         New line'
  echo -E '\r         Carriage return'
  echo -E '\t         Horizontal tab'
  echo -E '\v         Vertical tab'
  echo -E "\'         Single quotation mark"
  echo -E '\"         Double quotation mark'
  echo -E '\\         Backslash'
  echo -E '\?         Literal question mark'
  echo -E '\uhhhh     Unicode character'
  echo -E '\Uhhhhhhhh Unicode character'
  echo -E '\xhh       ASCII character in hexadecimal notation'
  echo -E '\xhhhh     Unicode character in hexadecimal notation if this escape sequence is used in a wide-character constant or a Unicode string literal.'
}

test_true_color() {
  # Run this to test if your terminal emulator supports True Color
  curl --silent https://raw.githubusercontent.com/JohnMorales/dotfiles/master/colors/24-bit-color.sh | bash
}

test_16_colors() {
  # Useful for testing your terminal's theme.
  echo -e "Normal: ${black}black${red}red${green}green${yellow}yellow${blue}blue${magenta}magenta${cyan}cyan${white}white"
  echo -e "Bright: ${bright_black}black${bright_red}red${bright_green}green${bright_yellow}yellow${bright_blue}blue${bright_magenta}magenta${bright_cyan}cyan${bright_white}white"
}

test_text_formatting() {
  # Useful when appraising a Tmux. Things can get strange with true colors and
  # font rendering.
  echo -e "\e[1mbold\e[0m"
  echo -e "\e[3mitalic\e[0m"
  echo -e "\e[4munderline\e[0m"
  echo -e "\e[9mstrikethrough\e[0m"
}

test_unicode() {
  # Run this to test if your terminal supports unicode character rendering.
  echo -e '\u2600 \u2601 \u2602 \u2603 \u2604 \u2605 \u2606 \u2607 \u2608 \u2609 \u260A'
  echo -e '\u260B \u260C \u260D \u260E \u260F \u2610 \u2611 \u2612 \u2613 \u2614 \u2615'
  echo -e '\u2616 \u2617 \u2618 \u2619 \u261A \u261B \u261C \u261D \u261E \u261F \u2620'
  echo -e '\u2621 \u2622 \u2623 \u2624 \u2625 \u2626 \u2627 \u2628 \u2629 \u262A \u262B'
  echo -e '\u262C \u262D \u262E \u262F \u2630 \u2631 \u2632 \u2633 \u2634 \u2635 \u2636'
  echo -e '\u2637 \u2638 \u2639 \u263A \u263B \u263C \u263D \u263E \u263F \u2640 \u2641'
  echo -e '\u2642 \u2643 \u2644 \u2645 \u2646 \u2647 \u2648 \u2649 \u264A \u264B \u264C'
  echo -e '\u264D \u264E \u264F \u2650 \u2651 \u2652 \u2653 \u2654 \u2655 \u2656 \u2657'
  echo -e '\u2658 \u2659 \u265A \u265B \u265C \u265D \u265E \u265F \u2660 \u2661 \u2662'
  echo -e '\u2663 \u2664 \u2665 \u2666 \u2667 \u2668 \u2669 \u266A \u266B \u266C \u266D'
  echo -e '\u266E \u266F \u2670 \u2671 \u2672 \u2673 \u2674 \u2675 \u2676 \u2677 \u2678'
  echo -e '\u2679 \u267A \u267B \u267C \u267D \u267E \u267F \u2680 \u2681 \u2682 \u2683'
  echo -e '\u2684 \u2685 \u2686 \u2687 \u2688 \u2689 \u268A \u268B \u268C \u268D \u268E'
  echo -e '\u268F \u2690 \u2691 \u2692 \u2693 \u2694 \u2695 \u2696 \u2697 \u2698 \u2699'
  echo -e '\u269A \u269B \u269C \u269D \u269E \u269F \u26A0 \u26A1 \u26A2 \u26A3 \u26A4'
  echo -e '\u26A5 \u26A6 \u26A7 \u26A8 \u26A9 \u26AA \u26AB \u26AC \u26AD \u26AE \u26AF'
  echo -e '\u26B0 \u26B1 \u26B2 \u26B3 \u26B4 \u26B5 \u26B6 \u26B7 \u26B8 \u26B9 \u26BA'
  echo -e '\u26BB \u26BC \u26BD \u26BE \u26BF \u26C0 \u26C1 \u26C2 \u26C3 \u26C4 \u26C5'
  echo -e '\u26C6 \u26C7 \u26C8 \u26C9 \u26CA \u26CB \u26CC \u26CD \u26CE \u26CF \u26D0'
  echo -e '\u26D1 \u26D2 \u26D3 \u26D4 \u26D5 \u26D6 \u26D7 \u26D8 \u26D9 \u26DA \u26DB'
  echo -e '\u26DC \u26DD \u26DE \u26DF \u26E0 \u26E1 \u26E2 \u26E3 \u26E4 \u26E5 \u26E6'
  echo -e '\u26E7 \u26E8 \u26E9 \u26EA \u26EB \u26EC \u26ED \u26EE \u26EF \u26F0 \u26F1'
  echo -e '\u26F2 \u26F3 \u26F4 \u26F5 \u26F6 \u26F7 \u26F8 \u26F9 \u26FA \u26FB \u26FC'
  echo -e '\u26FD \u26FE \u26FF'
}

test_emojis() {
  # Outputs a few emojis to see if your terminal supports colored or
  # monochromatic emojis.
  for n in {0..9}
  do
    echo -e -n "\U1F60$n"
  done
  echo # newline to clean output
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
  # NOTE: `nohup` ensures that if I close the terminal, I won't all kill the
  # browser. Maybe this is similar to calling `disown %<job_id>`. The redirect
  # to `/dev/null` ensures that no `nohup.out` file is created.
  nohup "$BROWSER" $@ >/dev/null 2>&1 &
}

lh() {
  # Opens http://localhost:<port> in your $BROWSER.
  # Usage: `lh 8080`
  # Here, in case it wasn't obvious, `lh` stands for "localhost".
  browse "http://localhost:$1"
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

loop() {
  # Continuously loop `command` every `sleep_amt` interval. `sleep_amt` defaults
  # to 1 second. Pass y/n for `should_clear` if you'd like to clear the screen
  # at the end of each iteration.
  # Usage: loop <command> <sleep_amt> <should_clear>
  local command=$1;
  local sleep_amt=${2:-1};
  local should_clear=${3:-n}

  # clear the screen before kicking things off
  if [ $should_clear = y ]; then
    clear
  fi

  while true; do
    eval $command && sleep $sleep_amt
    if [ $should_clear = y ]; then
      clear
    fi
  done
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
  # Find or create a Tmux session. This should work both from within Tmux or
  # outside of Tmux.
  local session_name="${1}"
  if ! tmux has-session -t "${session_name}" 2> /dev/null; then
    local oldTMUX="${TMUX}"
    unset TMUX
    tmux new -d -s "${session_name}" -n "${session_name}"
    export TMUX="${oldTMUX}"
    unset oldTMUX
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

# Google3
p4_filelog() {
  # Logs a file's Piper history. This is a convenience wrapper around
  # `p4 filelog`.
  # `filename` should be a relative path.
  # Usage: p4_filelog <filename>
  # Depends: p4
  p4 filelog "//depot/$(pwd | grep -P -o 'google3\/.+$')/$1"
}

citc_workspace() {
  # Returns the name of your current CitC workspace
  pwd | grep -o -P "$(whoami)\/[^\/]+"
}

codesearch() {
  # Attempts to open the current directory in Google's Code Search.
  local slug="$(pwd | grep -P -o 'google3\/.+$')"
  browse "https://cs.corp.google.com/piper///depot/$slug"
}

cider() {
  # Opens the current workspace and current directory in Google's Cider
  # Not very useful at the moment because it cannot jump to the current file or
  # line number. Should also support an Emacs integration at some point.
  google-chrome \
    --app="https://cider.corp.google.com/?ws=$(citc_workspace)"
}

# i3
focus() {
  # Focuses an i3 window by application name.
  i3-msg "[class=\"$1\"] focus" >/dev/null
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
