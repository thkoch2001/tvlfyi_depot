#!/usr/bin/env zsh

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
