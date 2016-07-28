# Documentation

## ViM Guide


# File Ops

`:w`: save.
`:q`: quit.
`:wq`: save and quit.
`:q!`: quit without saving.


# Pane Management

`:vs`: vertically split the current pane.
`:sp`: horizontally split the current pane.

`ctrl` + `w` + `h`: move left to next pane.
`ctrl` + `w` + `j`: move down to next pane.
`ctrl` + `w` + `k`: move up to next pane.
`ctrl` + `w` + `l`: move right to next pane.


### Insertion & Deletion

`x`: deletes the character beneath the cursor.
`shift` + `x`: acts as a backspace.

`shift` + `i`: insert at the beginning of a line.
`shift` + `a`: insert at the end of a line.

`c`: combines a delete operation with an insert operation.

`d` + `d`: deletes a line.
`c` + `c`: changes a line.

`shift` + `o`: inserts above the current line.
`o`: inserts below the current line.

#### Copy & Paste
`y` + `y`: copies the current line.
`p`: pastes whatever is on the clipboard beneath current line.
`shift` + `p`: pastes whatever is on the clipboard above current line.


### Search

`/`: search forward for a seqeuence.
`?`: search backward for a seqeuence.


### Movement

#### basics

`h`: left-arrow
`j`: down-arrow
`k`: up-arrow
`l`: right-arrow

`w`: moves forward one word.
`b`: moves backward one word.
`e`: moves to the end of a word.


#### zip-around

`*`: finds the next occurrence of the word that is beneath the cursor.
`#`: does the reverse.

`{`: move up a paragraph.
`}`: move down a paragraph.

`%`: jump to matching bracket.

`:<INT>`: jumps to the line number, `<INT>`.


### Visual
`shift` + `v`: enter full-line visual mode.
`v`: enter visual mode.


### Bonus: powerful sequences

`d` + `t` + `<ANY_CHARACTER>`: deletes until the next occurrence of
`<ANY_CHARACTER>`.

`c` + `i` + `<ANY_BRACKET>`: changes the inner content of the nearest
matching bracket.

`j` + `j`: map this key to the <Esc> function

