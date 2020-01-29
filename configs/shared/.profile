# Some programs read from ~/.profile for values. It's best to set environment
# variables here instead of in ~/.zshrc or similar files, which are sourced
# everytime a new shell is created. The ~/.profile, on the other hand, is
# typically sourced only once at login.

PATH+=":$HOME/bin"                            # personal
PATH+=":$HOME/.local/bin"                     # personal
PATH+=":$HOME/.cargo/bin"                     # Rust
PATH+=":$HOME/.rvm/bin"                       # Ruby
PATH+=":$HOME/n/bin"                          # JavaScript, et al
PATH+=":$HOME/.yarn/bin"                      # JavaScript, et al
PATH+=":$HOME/.yarn/global/node_modules/.bin" # JavaScript, et al
export PATH
systemctl --user import-environment PATH

# Taken from the EXWM configuration documentation.
if [ -z "$DISPLAY" -a "$(tty)" = '/dev/tty5' ]; then
  exec xinit -- vt05
fi

# Application preferences
export BROWSER=google-chrome-stable
export TERMINAL=alacritty # temporary; change this back to `st`
export VISUAL=emacsclient
export EDITOR=emacsclient
export ALTERNATE_EDITOR=nvim

# Rust integration. Keep here so programs like Emacs can read this value.
RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUST_SRC_PATH

# TODO: Decide if clipmenu is compatible with EXWM.
# Ensure clipmenu uses rofi instead of dmenu
# TODO: Remove this after fully supporting Emacs client for clipmenu.
export CM_LAUNCHER=rofi

# Application configuration
export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude ".git"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Prevent compton from fading to the lock screen. This also prevents the white
# overlay that compton was causing to appear. Still unsure why that was
# happening.
# For more information, see the following GitHub issue:
# https://github.com/google/xsecurelock/issues/28
export XSECURELOCK_NO_COMPOSITE=1

# This fixes nixpkgs that rely on glibc-2.27, which allegedly breaks locale
# issues.
# See this thread for more details: https://github.com/NixOS/nixpkgs/issues/8398
LOCALE_ARCHIVE=$(readlink ~/.nix-profile/lib/locale)/locale-archive
export LOCALE_ARCHIVE

# Set environment variables for Nix. Don't run this for systems running NixOS.
# TODO: Learn why I can't use the variables from ~/dotfiles/.envrc.
case $(hostname) in
  zeno.lon.corp.google.com) . ~/.nix-profile/etc/profile.d/nix.sh;;
  seneca) . ~/.nix-profile/etc/profile.d/nix.sh;;
  wpcarro.c.googlers.com) . ~/.nix-profile/etc/profile.d/nix.sh;;
esac
