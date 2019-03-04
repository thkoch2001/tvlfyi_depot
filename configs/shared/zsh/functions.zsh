# From Google's ZSH Hacks
# NOTE: this file has since been modified by me.

# Improvement to fasd's existing `zz` alias
unalias zz
zz() {
  # TODO: Add documentation
  local dir
  dir="$(fasd -Rdl "$1" | fzf --query="$1" -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

fv() {
  # Usage: fv file pattern
  # This is useful when you know the fuzzy name of the file you want to edit
  local file
  file="$(fzf --exact --height 40% --reverse --query="$1"  --select-1 --exit-0)"
  [[ -n "$file" ]] && vim "$file"
}

tb() {
  # Toggle between blaze-bin and your source.
  # Useful if you like to cd into the dir where your source lives.
  if [[ $PWD =~ '(.*)/blaze-bin(.*)' ]]; then
    cd "${match[1]}${match[2]}"
  else
    cd "${PWD/\/google3//google3/blaze-bin}"
  fi
}

tj() {
  # Toggle between the source dir and test dir.
  if [[ $PWD =~ '(.*)/javatests(.*)' ]]; then
    cd "${match[1]}/java${match[2]}"
  else
    cd "${PWD/\/google3\/java//google3/javatests}"
  fi
}
