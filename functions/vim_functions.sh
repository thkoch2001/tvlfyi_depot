#!/usr/bin/env bash

# Easily search for strings within the files within the current directory.
# Specify file extensions and directories to exclude to improve accuracy.
# The file selected from fzf will be opened in vim.
function vfzopen() {
  echo -n "Search Term: "
  read search_query
  echo -n "Filename: "
  read filetype
  echo -n "Exclude Directory: "
  read exclude_dir

  if [ ! -z "$exclude_dirs" ]; then
    filename=$(find . -type f -name "$filetype" | \
       xargs grep -l "$search_query" | fzf-tmux)
  else
    filename=$(find . -type f -name "$filetype" -not -path "./${exclude_dir}/*" \
       | xargs grep -l "$search_query" | fzf-tmux)
  fi


  if [ ! -z "$filename" ]; then
    echo "$filename"
    vim +/"$search_query" "$filename"
    return 0
  else
    return 1
  fi
}
