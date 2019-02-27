# dotfiles

I'm documenting this primarily for personal use. This repository contains shell
configs, vim configs, emacs configs, a list of commonly used applications, and
other items.

Configuration is everything.


# SSHFS

SSHFS enables seamless file transfers from your local machine to a remote
machine.

To install, run:

```bash
$ brew cask install osxfuse
$ brew install sshfs
```

Assuming your remote machine is configured in your `~/.ssh/config` (see above),
you can mount your remote machine's home directory on your local machine like
so:

```bash
$ mkdir ~/ec2
$ sshfs ec2:/home/ubuntu ~/ec2 -o reconnect,follow_symlinks
```

Now your remote machine's home directory can be accessed using the `~/ec2`
directory. This directory can be transparently treated as if it were an ordinary
local directory. To illustrate how easy it is to use, let's install `Vundle`, a
Vim package manager, on our remote machine.

```bash
$ git clone https://github.com/VundleVim/Vundle.vim.git ~/ec2/.vim/bundle/Vundle.vim
```

Voila! We now have `Vundle` installed on our ec2 instance without needing to SSH
into that machine ourselves. That's all there is to it.


# GnuPG

  1. Download public key from keyserver. `gpg --receive-keys [KEY_ID]`
  2. Transfer backed-up private key information from secure disk
  3. Create `[E]` encrypting and `[S]` signing subkeys for personal computer

## Commentary

By default `gpg2` interfaces with `gpg-agent`. `gpg` does not unless
`--use-agent` is specified.  I suggest using `gpg2`, but if you must use `gpg`,
add the following entry to `~/.gnupg/gpg.conf`:

```
use-agent
```

## GnuPG + Git

  1. Register newly created `[S]` signing subkey as `signingkey`
  2. Enforce commit-signing
  3. Opt into `gpg2` usage

```bash
$ git config --global user.signingkey <SIGNING_KEY>
$ git config --global commit.gpgsign true
$ git config --global gpg.program gpg2
```

## GnuPG + GPG-Agent

Setup `gpg-agent` to use password caching by adding the following entries to
`~/.gnupg/gpg-agent.conf`:

```
default-cache-ttl 300 max-cache-ttl 3600
```


# Neovim

The following snippet fixes the `<C-h>` issue in neovim on macOS.

```
$ infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
$ tic $TERM.ti
```


## True Color and Italics in tmux and vim

### TrueColor

Note: make sure that the terminal you are using supports TrueColor (hint: recent
version of iTerm2 do). Also make sure that the tmux version you are using
supports TrueColor (hint: versions north of 2.2 should).

At each step of the way, test TrueColor using the following shell pipeline
(hint: the gradients should be smooth):

```bash
$ curl https://raw.githubusercontent.com/JohnMorales/dotfiles/master/colors/24-bit-color.sh | bash
```

* Terminal: recent versions of iTerm 2 should support TrueColor
* Tmux: versions 2.2 and after should support TrueColor
* NeoVim: recent versions of NeoVim should support TrueColor

Enable TrueColor in your `~/.vimrc` (already done in this repository):

```viml
set termguicolors
```


Enable TrueColor in your `~/.tmux.conf` (already done in this repository):

Note: This may conflict with the setting for italics. Need to verify to confirm
/ disconfirm this (pending).

```
set -ga terminal-overrides ",xterm-256color-italic:Tc"
```


### Italics

In the file `/configs/shared/.tmux.conf` there is a line to add italics support
to tmux:

```
set -g default-terminal "tmux-256color-italic"
```

The `$TERM` entry, `tmux-256color-italic`, will be unavailable until you add the
file, `tmux-256color-italic`, to your terminal database. You can do this with
the following command:

```bash
$ tic ~/dotfiles/tmux-256color-italic
```


### Powerline

Install Powerline...

```bash
$ pip install powerline-status
```

Install the Powerline fonts...

```bash
$ hub clone 'powerline/fonts'
$ cd fonts && ./install.sh && cd ../ && rm -rf fonts
```

Lastly, ensure that the line in `.tmux.conf` that sources the `powerline.conf`
is uncommented:

```
run-shell "powerline-daemon -q"
source "/usr/local/lib/python2.7/site-packages/powerline/bindings/tmux/powerline.conf"
```


## Commonly used applications (Mac)

Thankfully `brew cask` simplifies the installation of many of my commonly used
applications:

```bash
$ brew cask install alfred dash slack 1password slack emacs dropbox iterm2 flux docker
```

The following applications need to be downloaded / installed manually:

* oh-my-zsh: a full suite of z-shell extensions
* homebrew: CLI for procuring third-party applications
* slate.js: resize and move your windows with keyboard shortcuts
* google chrome: web browser


## Commonly used fonts
* Install [Hasklig](https://github.com/i-tu/Hasklig) for ligature support in Elm, Elixir, etc
* Install [Operator Mono](http://www.typography.com/blog/introducing-operator) for expressive monospaced font
* Install powerline fonts
* Install Adobe Source Code Pro font for shell and text editors

### Ligature Support

To support ligatures make sure Hasklig is installed (link at the bottom). Ensure
that you are using an terminal emulator that supports ligatures. With both of
these tasks completed, ligatures should function in Neovim.


## Miscellaneous notes
* Map `<CAPS_LOCK>` key to `<ESC>`
* Increase key-repeat rate
* Decrease key-repeat-delay
* Increase mouse speed
