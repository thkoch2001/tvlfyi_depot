# Simple script designed to be `source-file`'d from within Tmux. It will setup
# panes with common directories for work.

# Dotfiles
new -s dotfiles -n main
send-keys 'cd ~/dotfiles' C-m

# Meta
new -s meta -n main
send-keys 'cd ~/urbint/meta' C-m

# Grid
new -s grid -n main
send-keys 'cd ~/urbint/grid' C-m
