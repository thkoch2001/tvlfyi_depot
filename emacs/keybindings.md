# Keybindings

Since I'm using Emacs to manage most of my workflow, all of the keybindings
should be defined herein and -- in order to scale -- order must be imposed. This
can help avoid KBD collisions and improve my ability to remember each KBD.

See `kbd.el` for the programmatic encoding of these principles.

## Troubleshooting

When in doubt, use Emacs's `read-key` and `read-event` to learn what signal
you're sending Emacs.

### Super-

- EXWM X11 windows are not processing `s-`.
- EXWM X11 windows are not processing `<M-ESC>`.

### Super-Ctrl-

I'm reserving `C-s-` for opening X11 applications.

- `terminator`: `t`
- `google-chrome`: `c`

## Emacs nouns

Most of my keybindings should be organized according to their function, which in
turn should be related to the following Emacs nouns.

- `workspace`: As defined by EXWM.
- `frame`: What non-Emacs users would call a "window". Currently my workflow
  doesn't use or rely on Emacs frames.
- `window`: A vertical or horizontal split within an Emacs frame.
- `buffer`: Anything storing text in memory.

## Prefixes and their meanings

TODO: Have a system for leader-prefixed KBDs, chords, and prefixed chords.

- `s-`: Switching between named workspaces. Right now, super is too overloaded
  and would benefit from having more deliberate keybindings.
- `C-M-`: Window sizing
- `M-{h,j,k,l}`: Window traversing
- `M-{\,-}`: Window splitting
- `M-q`: Window deletion
- `<leader>-q`: Window deletion
