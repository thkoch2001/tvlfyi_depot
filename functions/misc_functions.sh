# fuzzily-find-file
function wgff {
  echo $(find . -type f | fzf)
}


# fuzzily-find-branch
function wgfb {
  echo $(git branch -a | fzf)
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
  dir=$(dirname $(fzf)) && pushd "$dir" >/dev/null
}


# pushd into a directory on your dirs stack
function wpushd {
  dir="$(dirs | tr ' ' '\n' | fzf)" && pushd "$dir"
}


# trims leading and trailing whitespace
function trim {
  input="$1"

  echo "${input//[[:blank:]]/}"
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

