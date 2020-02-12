# Dotfile Symlink Manager

Find and delete all symlinks to the dotfiles defined in `$BRIEFCASE`.

Oftentimes I corrupt the state of my configuration files. The intention with
this script is to help me clean things up when this happens. An example workflow
might look like:

```shell
> symlink-mgr --audit
> symlink-mgr --seriously
> briefcase # changes directory to $BRIEFCASE
> make install
```
