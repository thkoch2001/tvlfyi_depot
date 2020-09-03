set -gx GOPATH "$HOME/go"
set -gx GPG_TTY (tty)
set -gx DEPOT_ROOT "$GOPATH/src/code.tvl.fyi"

set -gx PATH '/usr/local/go/bin' "$HOME/.cargo/bin" "$HOME/.rbenv/bin" $PATH
eval (rbenv init -)
source ~/.opsrc.fish # work
set -gx PATH "$HOME/go/bin" $PATH
