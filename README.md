# My Mac Configuration
I'm documenting this for personal use. Shell settings, vim settings, commonly used applications, et cetera...

### to do
* migrate Google Chrome bookmarks to new machine

### Commonly used applications
* homebrew - `key-repeat: 50ms delay-until-repeat: 300ms` necessary for procuring shell applications
* karabiner - increase your Mac's key repeat settings beyond the default range
* spectacle - resize and move your windows with keyboard shortcuts
* iterm - substitute for Terminal application
* oh my zsh - z-shell for Mac
* sublime text - text editor
* webstorm - web IDE
* pycharm - python IDE
* docker - for running containers and virtual environments
* mou - markdown editor
* f.lux - modulates the blue amount from the screen throughout the day

### Sublime Text settings
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

#### Sublime preferences (user)

```
{
	"color_scheme": "Packages/User/SublimeLinter/Oceanic Next (SL).tmTheme",
	"font_face": "Source Code Pro",
	"font_size": 12,
	"ignored_packages":
	[
		"Vintage",
		"VintageousOrigami"
	],
	"tab_size": 2,
	"theme": "Oceanic Next.sublime-theme",
	"translate_tabs_to_spaces": true,
	"vintage_start_in_command_mode": true,
	"vintageous_use_ctrl_keys": true
}
```

### other notes
* add Adobe Source Code Pro font for shell and text editors
* increase trackpad / mouse speed to maximum levels
