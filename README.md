# briefcase

Welcome to my briefcase: my monorepo.

I'm attempting to amass a collection of packages that span a variety of
languages while minimizing the costs of sharing the code. This also includes
configuration for things like emacs, ssh, and other tools.

# Installation (Deprecated)

The installation instructions here are deprecated. I'd like to manage packaging
and installing with Nix, but that is only partially supported at the
moment.

## wpgtk and gvcci

```bash
$ apti python-pip3
$ gclone deviantfero/wpgtk
$ cd ..
$ gclone FabriceCastel/gvcci
```

- TODO: Integrate Emacs themes into wpgtk.
- TODO: Integrate Vim themes into wpgtk.
- TODO: add these to the install script

```bash
$ ln -s ~/Dropbox/.password-store ~/.password-store
$ ln -s ~/Dropbox/bin ~/bin
$ import_gpg $DOTFILES/configs/shared/gpg/.gnupg/exported
```

1. Clipmenu

Clipmenu is a service to store a history of copied strings.

Install it as:
```bash
$ cd ~/programming && g clone cdown/clipmenu
```

- TODO: Include `~/.config/systemd/user` in `configs/shared`.
- TODO: Obviate installation.

Ensure that it runs on startup:
```bash
$ cd ~/programming/clipmenu
$ cp clipmenu clipmenud clipdel ~/bin # You may not need to do this step.
$ vim init/clipmenud.service
# Change the ExecStart line to point to ~/bin/clipmenud
$ cp init/clipmenud.service ~/.config/systemd/user/clipmenud.service
$ systemctl --user start clipmenud
$ systemctl --user enable clipmenud # This step may be optional.
$ reboot
$ systemctl --user status clipmenud # Verify installation worked.
```

1. Install Dropbox

```bash
$ cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
$ crontab -e # add the following line...
@reboot ~/.dropbox-dist/dropboxd 2>&1 >/tmp/dropbox.log
$ reboot            # 1/3 verify installation
$ pgrep dropbox     # 2/3 verify installation
$ dropbox.py status # 3/3 verify installation
```

1. Authorize computer to access GitHub

```bash
$ ssh-keygen -t rsa -b 4096 -C 'wpcarro@gmail.com'
$ eval $(ssh-agent -s)
$ ssh-add ~/.ssh/id_rsa
$ xclip -sel clip <~/.ssh/id_rsa.pub
$ browse github.com # paste ssh public key in settings
```

1. Install Vundle, nix-env

```bash
$ ln -s ~/Dropbox/Vundle.vim ~/.config/nvim/bundle/Vundle.vim
$ cat ~/Dropbox/install_nix.sh | sh
$ for p in $(cat nix-env.txt); do
>   nix-env -i "$p"
> done
```

1. Install dotfiles

- TODO: include steps 2-4 in the `make install` command.

Missing the following dependencies:

- `stow`
- `neovim`
- `bat`
- `exa`
- `fasd`
- `opam`
- `ghcup`
- `ripgrep`
- `fzf`
- `fd`
- `hub`
- `pass`

```bash
$ cd ~/Dropbox/dotfiles
$ DOTFILES="$(pwd)" make install
```

1. Install Node dependencies

For now, this deserves its own section since it isn't automated.

```bash
$ gclone tj/n       # clone repo
$ sudo make install # build from source
$ n stable          # install the stable version of node
```

- TODO: support dependencies like terminal themes

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

To install GPG run the following:

```bash
$ import_gpg
```

TODO: create a job that runs this periodically.

```bash
$ export_gpg
```

## Reference

    - sec: secret key
    - pub: public key
    - ssb: secret sub-key
    - sub: public sub-key


## Terminals and Fonts

Any terminal or font I choose should pass the following checks:

```bash
$ test_true_color
$ test_16_colors
$ test_text_formatting
$ test_unicode
$ test_emojis
```

### Ligatures

If using a font with ligature (e.g. Hasklig) assert that your terminal also support ligatures.
