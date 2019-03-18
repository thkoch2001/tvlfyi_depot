# dotfiles

I'm documenting this primarily for personal use. This repository contains shell
configs, vim configs, emacs configs, a list of commonly used applications, and
other items.

Configuration is everything.


# Setting up new computer

1. Install Dropbox

```bash
$ cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
$ crontab -e # add the following line...
@reboot ~/.dropbox-dist/dropboxd 2>&1 >/tmp/dropbox.log
$ reboot            # 1/3 verify installation
$ pgrep dropbox     # 2/3 verify installation
$ dropbox.py status # 3/3 verify installation
```

1. Authorize computer to access dotfiles

```bash
$ ssh-keygen -t rsa -b 4096 -C 'wpcarro@gmail.com'
$ eval $(ssh-agent -s)
$ ssh-add ~/.ssh/id_rsa
$ xclip -sel clip <~/.ssh/id_rsa.pub
$ browse github.com # paste ssh public key in settings
$ mkdir ~/programming
$ git clone git@github.com:wpcarro/dotfiles ~/Dropbox/dotfiles
```

1. Install Antigen, Vundle, nix-env for package management

```bash
$ # antigen
$ curl -L git.io/antigen >~/antigen.zsh
$ # vundle
$ g clone VundleVim/Vundle.vim ~/.config/nvim/bundle/Vundle.vim
$ # nix-env
$ curl https://nixos.org/nix/install | sh
$ for p in $(cat nix-env.txt); do
>   nix-env -i "$p"
> done
```

1. Install i3

```bash
$ sudo apt-get install i3
```

1. Install dotfiles

TODO: include steps 2-4 in the `make install` command.

```bash
$ cd ~/Dropbox/dotfiles
$ DOTFILES="$(pwd)" make install
```


# TODOS

- support dependencies like terminal themes


# SSHFS

TODO: add explanation about `unison`, `rsync`, etc.

SSHFS enables seamless file transfers from your local machine to a remote
machine.

## Usage

Assuming your remote machine is configured in your `~/.ssh/config` (see above),
you can mount your remote machine's home directory on your local machine like
so:

```bash
$ mkdir ~/ec2
$ sshfs ec2:/home/ubuntu ~/ec2 -o reconnect,follow_symlinks
```

Now your remote machine's home directory can be accessed using the `~/ec2`
directory. This directory can be treated as if it were an ordinary local
directory. To illustrate how easy it is to use, let's install `Vundle` onto our
remote machine.

```bash
$ git clone https://github.com/VundleVim/Vundle.vim.git ~/ec2/.vim/bundle/Vundle.vim
```

Voila! We now have `Vundle` installed on our ec2 instance without needing to
manually SSH into that machine.


# GnuPG

Entering a new system?

```bash
$ ./configs/shared/gpg/.gnupg/import.sh path/to/directory
```

Leaving an old system? TODO: create a job that runs this periodically.

```bash
$ ./configs/shared/gpg/.gnupg/export.sh [directory]
```

## Reference

    - sec: secret key
    - pub: public key
    - ssb: secret sub-key
    - sub: public sub-key

## GnuPG + Git

  1. Register newly created `[S]` signing subkey as `signingkey`
  1. Enforce commit-signing
  1. Opt into `gpg2` usage

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
* Install executables or scripts to `~/bin`
  * should be fine as long as they are shared between computers with i386 architectures
* Map `<CAPS_LOCK>` key to `<ESC>`
* Increase key-repeat rate
* Decrease key-repeat-delay
* Increase mouse speed
