# su

su (sterni's C utilities) contains reusable C code I've accumulated
while writing mostly HTTP and CGI related stuff (as you can probably
tell from the scope of included stuff). It is built around `<su/s.h>`,
a simple and safe-ish library for strings located on the heap.

The complete interface is prefixed with `su_` except for `<su/s.h>`
(string manipulation should be brief and fun, I figured).

## constituents

* `<su/s.h>`: strings (`s_t`) and slices (`slice_t`)

* `<su/char.h>`: (very incomplete) ascii toolkit

* `<su/check.h>`: laughably simple unit testing framework

* `<su/json/render.h>`: easy to use json rendering

* `<su/memstream.h>`: convenient wrapper around `open_memstream(3)`

* `<su/url.h>`: url{en,de}coding

* `<su/xml/render.h>`: easy to use xml rendering

## API documentation

work in progress
