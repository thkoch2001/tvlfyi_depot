# NOTE: All functions should be documented. Including usage examples.
# NOTE: Prioritize the error handling (especially error messages) as highly as
# most people prioritize "happy-paths" (aka features).

################################################################################
# Personal dependencies
################################################################################

source ~/Dropbox/programming/db_cli/src/index.zsh


################################################################################
# Documentation and error messages
################################################################################

# TODO: Move these to their own repository.

echo_info() {
  # Echos an informational message.
  #
  # depends_variable blue
  echo -e "${blue}[INFO]: $1"
}

echo_warn() {
  # Echos a warning message.
  # This function depends on the colors defined in variables.zsh.
  #
  # depends_variable yellow
  echo -e "${yellow}[WARNING]: $1"
}

echo_error() {
  # Echos an error message.
  #
  # depends_variable red
  echo -e "${red}[ERROR]: $1"
}

unsupported_input() {
  # Generic error message. Consume herein to standardize the error messages.
  # Pass the supported inputs as $1.
  #
  # depends error_error
  echo_error "Unsupported input. This function only supports the following inputs: $1. Exiting..."
}

depends() {
  # Prints a message explaining a function's dependencies. Consume here to
  # standardize the error messages.
  # Pass the dependencies as $1.
  #
  # depends echo_info
  echo_info "This function depends on the following functions: $@"
}

depends_variable() {
  # Prints a message explaining a dependency on a variable. Consume here to
  # standardize the error messages.
  # Pass the dependencies as $1.
  #
  # depends echo_info
  echo_info "This function depends on the following variables: $@"
}

depends_alias() {
  # Prints a message explaining a dependency on a shell alias. Consume here to
  # standardize the error messages.
  # Pass the dependencies as $1.
  #
  # depends echo_info
  echo_info "This function depends on the following aliases: $@"
}

compliments() {
  # Prints a message explaining that a function compliments another function.
  # Think of complimentary functions as `zip` and `unzip`.
  #
  # depends echo_info
  echo_info "This function compliments the \`$1\` function."
}


################################################################################
# Filesystem operations
################################################################################

ensure_dir() {
  # Ensures that the directory and its children exist.
  # Usage: ensure_dir <path-to-dir>
  mkdir -p $1
}

ensure_file() {
  # Ensures that the file and the path to that file exist.
  # Usage: ensure_dir <path-to-file>
  # depends ensure_dir
  ensure_dir $(dirname $1) && touch $1
}

tar_dir() {
  # Tars dir as dir.tar. Removes dir.
  # compliments untar_dir
  tar -cf  "$1.tar" "$(basename $1)" && rm -rf "$1"
}

untar_dir() {
  # Untars dir.tar as dir. Removes dir.tar.
  # compliments tar_dir
  tar -xvf "$1" && rm "$1"
}

targz_dir() {
  # Tars a dir as dir.tar.gz.
  # compliments untargz_dir
  tar -czf "$1.tar.gz" "$(basename $1)"; rm -rf "$1"
}

untargz_dir() {
  # Untars dir.tar.gz as dir. Removes dir.tar.gz.
  # compliments targz_dir
  tar -xzvf "$1" && rm "$1"
}

zip_dir() {
  # Zips dir as dir.zip. Removes dir.
  # compliments unzip_dir
  zip -r "$1.zip" "$(basename $1)" && rm -rf "$1"
}

unzip_dir() {
  # Unzips dir.zip as dir. Removes dir.zip.
  # compliments zip_dir
  unzip "$1" && rm "$1"
}

archive() {
  # Generic function for archiving directories
  #
  # depends tar_dir targz_dir zip_dir
  # compliments unarchive
  printf "Which type of archive would you like to like create? (tar, tar.gz, zip) "
  case $(read -e) in
    tar)    tar_dir   "$1";;
    tar.gz) targz_dir "$1";;
    zip)    zip_dir   "$1";;
    *)      unsupported_input "tar, tar.gz, zip";;
  esac
}

unarchive() {
  # Generic way to unarchive files.
  # Currently supports the following extensions:
  # - .tar
  # - .tar.gz
  # - .zip
  #
  # depends untar unzip
  # compliments archive
  case $1 in
    *.tar.gz) untargz_dir "$1";;
    *.tar)    untar_dir   "$1";;
    *.zip)    unzip_dir   "$1";;
    *)        unsupported_input ".tar, .tar.zip, .zip"
  esac
}


################################################################################
# Filesystem operations
################################################################################

alert() {
  # Send the user information via the GUI.
  # Intended to have the same API as the Javascript alert function.
  # Usage: alert [msg-body]
  # depends notify-send
  notify-send 'Info' "$1"
}

alert_echo() {
  # Composes `echo` and `alert` together.
  # Usage: alert_echo [msg-body]
  # depends alert echo
  echo "$1" && alert "$1"
}


################################################################################
# Unclassified
################################################################################

deref() {
  # Dereferences a symlink.
  # Usage: deref [symlink]
  if ! [ -L $1 ]; then
    echo_error "File is not a symlink: $1. Exiting..."
  else
    local src=$(readlink -f $1)
    echo "Moving $src -> $1" && \
      mv $1 $1.bak && \
      mv $src $1 && \
      rm $1.bak
  fi
}

wallpaper() {
  # Select and load a wallpaper from the wallpaper directory.
  local files=$(ls ~wallpaper)
  local selection=$(echo $files | fzf)
  local fullpath=~wallpaper/$selection

  feh --bg-scale $fullpath
}

# TODO: Write more robust, tested dotfile manager application in Elisp.
dotfilify() {
  # Moves a regular, non-symlinked file into my dotfiles.
  # compliments undotfilify
  # depends ensure_dir
  local original_path=$(realpath $1)
  local dotfile_path="${DOTFILES}/configs/shared/${original_path#$HOME/}"
  ensure_dir $(dirname $dotfile_path) && \
    mv $original_path $dotfile_path && \
    ln --force -s $dotfile_path $original_path
}

# TODO: Write more robust, tested dotfile manager application in Elisp.
undotfilify() {
  # De-references a file that is symlinked to in my dotfiles.
  # Usage: undotfilify [path-to-symlink]
  # compliments dotfilify

  if ! [ -L "$1" ]; then
    echo_error "Not a symbolic link: $1. Exiting..."
    return 1
  else
    local src=$(readlink -f $1)

    echo "Removing: $1" && rm $1 && \
      echo "Moving: $src -> $1" && \
      mv $src $1
  fi
}

markdown() {
  # Simple way to read markdown on the command-line.
  # Usage: markdown [file]
  # depends pandoc less
  pandoc -t plain $1 | less
}

rofi_prompt() {
  # Simple prompt for user input using `rofi`.
  # Usage: rofi_prompt [label]
  local label="${1:-Input}"
  rofi -dmenu -p "$label" -theme-str 'listview { enabled: false; }'
}

import_gpg() {
  # Shorthand for executing the import script for my GPG creds.
  local gpg_config=$DOTFILES/configs/shared/gpg/.gnupg
  $gpg_config/import.sh $gpg_config/exported
}

export_gpg() {
  # Shorthand for executing the export script for my GPG creds.
  local gpg_config=$DOTFILES/configs/shared/gpg/.gnupg
  $gpg_config/export.sh $gpg_config/exported
}

create_bootable_usb() {
  # This was created primarily to document the bootable USB creation process, so
  # that I'm less dependent on internet solutions.
  # Warning this is experimental.
  printf 'Path to .iso: '
  local lf=$(read -e) # NOTE: maybe use `read lf` instead.
  printf 'Path to USB: '
  local of=$(read -e)

  sudo dd \
    bs=4M \
    if="${lf}"
    of="${of}"
    status=progress \
    iflag=sync
}

file_sizes() {
  # Table to help me conceptualize file sizes.
  echo "  1 kB\tHalf a page of raw text"
  echo "  8 kB\tLogo image"
  echo "500 kB\t5-page word processor document"
  echo "  1 MB\t1 minute MP3"
  echo "  5 MB\t3 minute MP3"
  echo "700 MB\tA full CD-ROM"
  echo "  4 GB\tA full DVD"
}

ldap() {
  # Returns the Google LDAP for `user`
  declare -A ldaps
  ldaps["ahmed"]=ahmedhegazy
  ldaps["arturo"]=arturog
  ldaps["daniel"]=dsipasseuth
  ldaps["dirichi"]=dirichi
  ldaps["jack"]=jackwootton
  ldaps["jon"]=jonmatthews
  ldaps["micheal"]=michealg
  ldaps["rose"]=roseanna
  ldaps["william"]=wpcarro

  echo ${ldaps["$1"]}
}

repl_closure() {
  # Creates a `node` REPL for users to test out Google's Closure library.
  # The naming `repl_closure` follows the repl_ convention that alias.sh
  # follows.
  docker build -t closure_repl - <~/programming/dockerfiles/closure_repl.docker
  docker run -it closure_repl:latest
}

checkout_cl() {
  # - find-or-create a new workspace named `cl-<number>`
  # - syncs the workspace to tip
  # - patches the CL ontop of tip
  hg citc "cl-$1" && \
  g4d "cl-$1" && \
  hg sync && \
  hg patch "cl/$1"
}

prodaccess() {
  # Wraps existing `prodaccess` command to provide Google-specific tips.
  # Take from this: https://g3doc.corp.google.com/experimental/users/diamondm/fortunes/README.md?cl=head
  command prodaccess "$@" && \
    cowsay $(/google/data/ro/users/di/diamondm/engfortunes/fortune.sh --extra_space)
}

home_theater() {
  # Does the following:
  #   - connects to NordVPN for HBO
  #   - connects to the TV via the attached HDMI cable
  #   - connects to the bluetooth speaker

  # `nordvpn` is an alias defined in `aliases.zsh`
  echo 'Run the following to stream HBOGo from this laptop to your TV:'
  echo '  1. nordvpn'
  echo '  2. xrandr --output HDMI1 --mode 3840x2160'
  echo '  3. bluetoothctl'
  echo '  4. connect CC:6E:A4:32:6B:BB'
  echo
  echo 'Having trouble? Use the following tools to troubleshoot:'
  echo '  - blueman-manager'
  echo '  - pavucontrol'
  echo '  - gnome-control-center'
}

create_citc() {
  # Creates an `hg` citc client and `cd`s to its root.
  # Usage: create_citc esc-b-119275355
  # Note: May want to get into the habit of naming citc clients after the
  # Buganizer tickets. The buganizer portion can be prefixed with the shorthand
  # representation of the project it relates to. This coupling could lead to
  # more integrated tooling.
  hg citc $1 && g4d $1
}

fv() {
  # Usage: fv file pattern
  # This is useful when you know the fuzzy name of the file you want to edit
  local file
  file="$(fzf --exact --height 40% --reverse --query="$1"  --select-1 --exit-0)"
  [[ -n "$file" ]] && vim "$file"
}

project_root() {
  # Changes to the directory of the project root.
  if [[ "$(pwd)" =~ '(.*)/blaze-bin(.*)' ]]; then
    cd "${match[1]}${match[2]}"
  elif [[ "$(pwd)" =~ '(.*)/blaze-genfiles(.*)' ]]; then
    cd "${match[1]}${match[2]}"
  fi
}

blaze_bin() {
  # Changes to the blaze-bin directory.
  # Depends:
  #   - project_root
  project_root && cd "${PWD/\/google3//google3/blaze-bin}"
}

blaze_genfiles() {
  # Changes to the blaze-genfiles directory.
  # Depends:
  #   - project_root
  project_root && cd "${PWD/\/google3//google3/blaze-genfiles}"
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

screenshot() {
  # Ergonomic way to take a screenshot.
  # Writing this since I usually forget the command.
  # Usage: screenshot
  alert_echo 'Click-and-drag to select the region to capture.'
  local filepath=$(scrot --select '%Y-%m-%d_$wx$h.png' -e 'mv $f /tmp && echo /tmp/$f')
  c <<<$filepath
  alert_echo "Copied to clipboard!"
}

snipit() {
  # Take a screenshot and host it at https://screenshot.googleplex.com
  # Adapted from SnipIt to fit my workflow.
  # depends alert_echo
  server="https://screenshot.googleplex.com/upload"
  file="${TEMP:-/tmp}/snipit_temp_$$.png"

  # Capture
  alert_echo "SnipIt - Click a window, or drag to snip a region (Ctrl+C to cancel):" && \
    import "$file" && \
    echo "Sending image to server..." && \
    uri=$(curl -sF "imagedata=@$file" $server) && \
    c <<<"$uri" && \
    alert_echo "Copied to clipboard!"
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
  # Runs a Docker container interactively
  # Usage: dkrit <container_name> <command> [...args]
  docker run -it $@
}

# gist
gistp() {
  # Creates a gist with the contents of the clipboard.
  # Prompts the user for the filename and the descriptions of the gist.
  # Copies the Gist URL to the user's clipboard thereafter.
  #
  # depends_alias p
  # depends gist
  printf "Filename including extension: "
  read filename
  printf "Gist description: "
  read description
  p | gist -e -f $filename -d $description
}

# Github
gh_create() {
  # Create git repository in `~/Dropbox/programming`.
  # Push repo to my github account.
  # Usage: grepo [repo-name]
  # depends_alias mdd g
  # compliments gh_delete

  if [ $# -eq 0 ]; then
    echo 'You must supply the name for the repo. Exiting...'
    return 1
  else
    mdd "$HOME/Dropbox/programming/$1" && \
      g init && \
      g create
  fi
}

gh_delete() {
  # Deletes a repository from my Github.
  # compliments gh_create

  if [ $# -eq 0 ]; then
    echo 'You must supply the name for the repo to delete. Exiting...'
    return 1
  else
    g delete "$1"
  fi
}

# Git

gconflicts() {
  # Edit git conflicts one-by-one in your favorite editor.
  ${EDITOR} "$(git status --porcelain | awk '/^UU/ { print $2 }')"
}

gclone() {
  # Since I inevitably always call `cd` after `g clone`.
  # Usage: gclone cdown/clipmenu
  # depends_alias g
  # depends_alias la
  g clone "$1" && cd "${1#*/}"
}

# GPG
_do_encrypt() {
  # Helper  function for `encrypt`.
  # depends gpg targz_dir
  echo "Encrypting..."

  if [ -f $1 ]; then
    gpg --encrypt --recipient=wpcarro@gmail.com $1
    rm $1

  elif [ -d  $1 ]; then
    targz_dir $1
    # NOTE: recursion is nice here but it causes the echo statements to happen
    # twice. Because of this, we redirect to /dev/null.
    _do_encrypt "$1.tar.gz" >/dev/null
  fi

  echo "Done."
}

_do_decrypt() {
  # Helper function for `decrypt`.
  # depends gpg untargz_dir
  echo "Decrypting..."

  gpg --decrypt $1 2>/dev/null >"${1%.gpg}"
  rm $1

  # If the file ends with tar.gz, it was most like a directory that we targz'd
  # then encrypted.
  if [[ "${1%.gpg}" =~ \.tar.gz$ ]]; then
    untargz_dir "${1%.gpg}" >/dev/null
  fi

  echo "Done."
}

encrypt() {
  # Convenience function around encrypting files and directories.
  # Appends a .gpg extension and deletes the unencrypted source.
  # depends _do_encrypt
  for f in $@; do
    _do_encrypt $f
  done
}

decrypt() {
  # Convenience function around decrypting .gpg files and directories.
  # Deletes the original encrypted file with the .gpg extension.
  # depends _do_decrypt
  for f in $@; do
    _do_decrypt $f
  done
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
  # Return the balance of my Monzo bank account.
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

cp_dwim() {
  # Calls `cp -r` when a directory is specified, otherwise uses `cp`.
  # This is closer to the UX you expect in GUIs when you copy-and-paste files.
  if [ -d $1 ]; then
    command cp -r $@
  else
    command cp $@
  fi
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

lad() {
  # List only directories in a directory..
  # Usage: lad [directory]
  # depends fd
  (cd ${1:-.} && fd --hidden --maxdepth 1 --type d)
}

laf() {
  # List only files in a directory.
  # Usage: lad [directory]
  # depends fd
  (cd ${1:-.} && fd --hidden --maxdepth 1 --type f)
}

lal() {
  # List only links in a directory.
  # Usage: lad [directory]
  # depends fd
  (cd ${1:-.} && fd --hidden --maxdepth 1 --type l)
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

hgbrowse() {
  # Attempts to open the current directory in Google's Code Search.
  # Note: try and get this command supported by Fig
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

# C
runc() {
  # Compile and run $1. Pass $1 as file.c.
  # This is modelled after the `runhaskell` command.
  # Deletes the compiled binary after executing it.
  #
  # depends gcc
  gcc "$1" -o "${1%.c}" && "./${1%.c}" && rm "${1%.c}"
}

# Rust
runrust() {
  # Compile and run $1. Pass $1 as file.rs.
  # This is modelled after the `runhaskell` command.
  # Deletes the compiled binary after executing it.
  #
  # depends rustc
  rustc "$1" && "./${1%.rs}" && rm "${1%.rs}"
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


################################################################################
# Theming
################################################################################

colors() {
  # Outputs the wpgtk-generated color palette annotated with numbers.
  echo '  0   1   2   3   4   5   6   7' && \
    wpg --preview && \
    echo '  8   9  10  11  12  13  14  15'
}

gvcci() {
  # Integrates `gvcii` and `wpgtk`.
  # Usage: gvcii path/to/wallpaper.jpg
  local filename="$(basename $1)"
  local directory="${filename%.*}"
  local json=~/.gvcci/themes/$directory/json-scheme.json

  if [ -f $json ]; then
    wpg --theme $json
  else
    (cd ~/Dropbox/programming/gvcci && \
       ./gvcci.sh "$1" && \
       wpg --theme $json)
  fi

  # TODO: Why do I need this?
  sleep 0.1 && feh --bg-scale $1
}
