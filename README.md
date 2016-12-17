# My Mac Configuration
I'm documenting this primarily for personal use. This reposity contains shell configs, vim configs, emacs configs, a list of commonly used applications, and other items.

The overall goal of this repository is to reduce the time it takes to adopt a new computer and equip it with the necessary tooling to do meaningful work.


# Neovim

The following snippet fixes the `<C-h>` issue in neovim on macOS.

```
$ infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
$ tic $TERM.ti
```


## Italics in tmux and vim

In the file `/configs/.tmux.conf` there is a line to add italics support to tmux:

```
set -g default-terminal "tmux-256color-italic"
```

The `$TERM` entry, `tmux-256color-italic`, will be unavailable until you add the file, `/configs/tmux-256color-italic`, to your terminal database. You can do this with the following command:

```bash
$ tic ~/pc_settings/configs/tmux-256color-italic
```


## Ligature Support

To support ligatures make sure Hasklig is installed (link at the bottom). Ensure that you are using an iTerm 2 build that supports ligatures. With both of these tasks completed, ligatures should function in Neovim.


### Commonly used applications
* homebrew - necessary for procuring shell applications
* karabiner - `key-repeat: 50ms delay-until-repeat: 300ms` increase your Mac's key repeat settings beyond the default range
* spectacle - resize and move your windows with keyboard shortcuts
* iterm - substitute for Terminal application
* oh my zsh - a full suite of z-shell extensions
* sublime text - text editor
* webstorm - web IDE
* pycharm - python IDE
* docker - for running containers and virtual environments
* mou - markdown editor
* f.lux - modulates the blue amount from the screen throughout the day

### Sublime Text Packages
* SublimeLinter - lints files
* SublimeLinter-contrib-eslint - lints using eslint
* Oceanic Next Theme - buffer and editor theme
* Vintageous - extended Vim key-binding support
* Vintageous-Origami - extended Vintageous support for window pane mgt (ctrl + w) `"vintageous_use_ctrl_keys": true`
* Origami - dep. of Vintageous-Origami (see above)
* babel-sublime - supports JSNext features
* SublimeCodeIntel - robust autocomplete engine
* PackageResourceViewer - quickly view / edit installed Sublime Packages to enhance customization options
* SidebarEnhancements - extensions for Sublime Text's sidebar
* SublimeREPL - convenient REPL for ST
* FoldComments - collapse comments with keystrokes

#### Sublime preferences (user)

[Preferences.sublime-settings](https://github.com/wpcarro/pc_settings/blob/master/Preferences.sublime-settings)

#### Sublime keybindings (user)

[Default (OSX).sublime-keymap](https://github.com/wpcarro/pc_settings/blob/master/Default%20(OSX).sublime-keymap)

### other notes
* Install [Hasklig](https://github.com/i-tu/Hasklig) for ligature support in Elm, Elixir, etc
* Install [FiraCode](https://github.com/tonsky/FiraCode/wiki) for ligature support
* Install [Operator Mono](http://www.typography.com/blog/introducing-operator) for expressive Italics
* Install powerline fonts 
* add Adobe Source Code Pro font for shell and text editors
* increase trackpad / mouse speed to maximum levels
