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
