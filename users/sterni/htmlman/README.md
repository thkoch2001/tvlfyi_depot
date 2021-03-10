# htmlman

static site generator for man pages intended for
rendering man page documentation viewable using
a web browser.

## usage

```nix
{ depot, ... }:

depot.users.sterni.htmlman {
  title = "foo project";
  pages = [
    {
      name = "foo";
      section = 1;
    }
    {
      name = "foo";
      section = 3;
      path = ../devman/foo.3;
    }
  ];
  manDir = ../man;
}
```
