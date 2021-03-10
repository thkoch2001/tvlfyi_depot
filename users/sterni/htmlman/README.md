# htmlman

static site generator for man pages intended for
rendering man page documentation viewable using
a web browser.

## usage

If you have a nix expression, `doc.nix`, like this:

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

You can run the following to directly deploy the resulting
documentation output to a specific target directory:

```sh
nix-build -A deploy doc.nix && ./result target_directory
```
