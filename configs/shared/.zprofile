# ZSH seems to avoid sourcing ~/.profile during SSH sessions.
# X (or something else) seems to source ~/.profile with it initializes.
# Putting this here allows my SSH sessions to have the expected configuration.
#
# Putting this here instead of in my .zshrc or .zshenv is advantageous since it
# is only called once when a terminal is created; subshells and nested ZSH
# sessions don't re-source this file.
#
# See this thread for more information: https://groups.google.com/a/google.com/forum/#!topic/zsh-users/VO2lEJRfFzk
source ~/.profile

export PATH="$HOME/.cargo/bin:$PATH"
