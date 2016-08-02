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

