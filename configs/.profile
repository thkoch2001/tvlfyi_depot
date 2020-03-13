# Some programs read from ~/.profile for values. It's best to set environment
# variables here instead of in ~/.zshrc or similar files, which are sourced
# everytime a new shell is created. The ~/.profile, on the other hand, is
# typically sourced only once at login.

# Taken from the EXWM configuration documentation.
if [ -z "$DISPLAY" -a "$(tty)" = '/dev/tty5' ]; then
  exec xinit -- vt05
fi

# I tried configuring home-manager to generate the ssh-agent init code (i.e. the
# code below), but it seems that the home-manager community support using
# gpg-agent to emulate the ssh-agent. I tried that, but I didn't fully
# understand the benefits.
eval "$(ssh-agent -s)"

# This fixes nixpkgs that rely on glibc-2.27, which allegedly breaks locale
# issues.
# See this thread for more details: https://github.com/NixOS/nixpkgs/issues/8398
export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"

# Set environment variables for Nix. Don't run this for systems running NixOS.
# TODO: Learn why I can't use the variables from ~/briefcase/.envrc.
case $(hostname) in
  zeno.lon.corp.google.com) . ~/.nix-profile/etc/profile.d/nix.sh;;
  seneca) . ~/.nix-profile/etc/profile.d/nix.sh;;
  wpcarro.c.googlers.com) . ~/.nix-profile/etc/profile.d/nix.sh;;
esac
