# My Mac Configuration
I'm documenting this for personal use. Shell settings, vim settings, commonly used applications, et cetera...

# Neovim

The following snippet fixes the `<C-h>` issue in neovim on macOS.

```
$ infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
$ tic $TERM.ti
```

### Commonly used applications
* homebrew - necessary for procuring shell applications
* karabiner - `key-repeat: 50ms delay-until-repeat: 300ms` increase your Mac's key repeat settings beyond the default range
* spectacle - resize and move your windows with keyboard shortcuts
* iterm - substitute for Terminal application
* oh my zsh - z-shell for Mac
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
