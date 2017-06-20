# fuzzily-find-branch
function wgfb {
  echo $(git branch -a | fzf-tmux)
}


# download files to /tmp directory
function wdownload {
  URL="$1"
  FILENAME="$(basename $URL)"

  wget -O /tmp/"$FILENAME" $URL >/dev/null && open /tmp && echo "Downloaded to: /tmp/$FILENAME" || echo "Error ..."
}


# spell checker
function wspcheck {
  if [ $# -ge 1 -a -f "$1" ] && input="$1" || input="-"
  cat "$input" | tr '[:upper:]' '[:lower:]' | tr -cd '[:alpha:]_ \n' | tr -s ' ' '\n' | sort | comm -23 - ~/english_words.txt
}


# fuzzily search through dirs stack
function wfd {
  dir=$(dirname $(fzf-tmux)) && pushd "$dir" >/dev/null
}


# pushd into a directory on your dirs stack
function wpushd {
  dir="$(dirs | tr ' ' '\n' | fzf-tmux)" && pushd "$dir"
}


# trims leading and trailing whitespace
function trim {
  input="$1"

  echo "${input//[[:blank:]]/}"
}


# Extends `codemod` to exclude dirs in .gitignore file
function cm {
  extensions="$1"
  regex="$2"
  replacement="$3"

  ignore_dirs=""

  if [ -f ./.gitignore ]; then
    # Sanitizes .gitignore and converts it to a comma-separated list
    ignore_dirs="$(sed 's/^\//.\//g' <./.gitignore | sed -e 's/#.*$//' -e '/^$/d' | tr '\n' ',' | sed 's/,$//')"
  fi

  codemod -m -d . --extensions ${extensions} --exclude-paths ${ignore_dirs} ${regex} ${replacement}
}


function tt() {
  sessionName="${1}"
  if ! tmux has-session -t "${sessionName}" 2> /dev/null; then
    oldTMUX="${TMUX}"
    unset TMUX
    tmux new -d -s "${sessionName}"
    export TMUX="${oldTMUX}"
    unset oldTMUX
  fi
  if [[ -n "${TMUX}" ]]; then
    tmux switch-client -t "${sessionName}"
  else
    tmux attach -t "${sessionName}"
  fi
}


# generates placeholder content for FE work
function lorem {
    text="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

    echo $text
}


# generates lorem and adds to pasteboard
function loremcp {
    lorem | pbcopy
}


# searches all PATH directories for a keyword
function wsearchpath {
  echo $PATH | tr ':' '\n' | xargs -I {} find {} -type f -perm +111 -maxdepth 1 -name "*${1}*" -print | xargs basename
}


# tests an internet connection
function is_online {
  wget -q --spider "http://google.com"

  if [ $? -eq 0 ]; then
    echo "Online"
    return 0
  else
    echo "Offline"
    return 1
  fi
}
