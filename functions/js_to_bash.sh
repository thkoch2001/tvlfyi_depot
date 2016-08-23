# js function syntax for bash

function trimend {
    LENGTH=${#1}
    AMT=$2
    TAKE=$((LENGTH-AMT))
    echo $1 | cut "-c-$TAKE"
}

function trimfront {
    TMP0=$(echo $1 | rev)
    TMP1=$(trimend $TMP0 $2 | rev)
    echo $TMP1
}

function length {
    echo ${#1}
}

function slice {
  start="$1"
  end="$2"
  string="$3"
  echo "${string:${start}:${end}}"
}

function setInterval {
  eval "while true; do $1; sleep $2; done"
}
