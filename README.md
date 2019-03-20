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

1. Install shared executables

Some files should be kept out of this repository. Things that come to mind
include secrets (e.g. `monzo_creds.json.gpg`) and binaries.

Secrets should be kept out of this repository in case. It might be okay to
include them herein since they should be encrypted. But just to add an
additional layer of security, I avoid adding them.

Executables are kept out of version control since they can be quite large. For
example, my `/usr/bin` is `8.9G` at the time of this writing. Also it can be
noisy to see diffs of binaries after upgrading programs.

While sharing binaries across systems feels brittle, everything should function
as long as the machines across which the binaries are shared run Linux and have
i386 architectures.

```bash
$ ln -s ~/Dropbox/bin ~/bin
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
`~/.gnupg/gpg-agent.conf` (already done in this repository):

```
default-cache-ttl 300 max-cache-ttl 3600
```


## True Color and Italics

At the time of this writing, Suckless's `st` terminal provides True Color and
italics support. It's also important to test that this support remains when
inside of Vim or inside of a Tmux session or both.

### TrueColor

To test for your terminal's True Color support, run:

```bash
$ test_true_color
```

Enable TrueColor in your `init.vim` (already done in this repository):

```viml
set termguicolors
```

### Italics

To test if your terminal supports italics and other text treatments, run:

```bash
$ test_text_formatting
```

### Ligatures

At the time of this writing, Suckless's `st` does not appear to support
ligatures.


## Miscellaneous notes
* Executables are shared at `~/Dropbox/bin` and not kept within this repository
  to avoid the bloat.
* Map `<CAPS_LOCK>` key to `<ESC>`
* Increase key-repeat rate
* Decrease key-repeat-delay
* Increase mouse speed
