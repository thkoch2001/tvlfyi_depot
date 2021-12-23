# Dotfile Symlink Manager

Find and delete all symlinks to my dotfiles.

Oftentimes I corrupt the state of my configuration files. The intention with
this script is to help me clean things up when this happens. An example workflow
might look like:

```shell
> symlink-mgr --audit
> symlink-mgr --seriously
```
