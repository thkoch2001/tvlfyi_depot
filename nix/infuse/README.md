# infuse.nix

## Why?

Would you rather write this:

```nix
pkgs // {
  xrdp = pkgs.(xrdp.override {
      pulseaudioSupport = false;
    })
    .overrideAttrs(previousAttrs: {
      env = previousAttrs.env // {
        NIX_CFLAGS_COMPILE =
          (previousAttrs.env.NIX_CFLAGS_COMPILE or "")
          + " -w";
      };
      passthru = previousAttrs.passthru // {
        xorgxrdp = previousAttrs.passthru.xorgxrdp
          .overrideAttrs (previousAttrs: {
            configureFlags = (previousAttrs.configureFlags or []) ++ [
              "--without-fuse"
            ];
          });
      };
    });
}
```

... or this?

```nix
infuse pkgs {
  xrdp.__input.pulseaudioSupport = false;
  xrdp.__output.env.NIX_CFLAGS_COMPILE.__append = " -w";
  xrdp.__output.passthru.xorgxrdp.__output.configureFlags.__append = ["--without-fuse"];
};
```

If you think the second expression is easier to read, write, and maintain, you
would probably be interested in `infuse.nix`.

## What?

The basic idea is that `infuse` acts like `lib.recursiveUpdate`, except that in
the second (right-hand) argument you must *mark* any subtrees where you want the
automatic merging to stop.  You mark those subtrees by changing them from
whatever value they were into a function which returns that value (and ignores
its argument).

In the following example, `fred` gets clobbered because we used `//`:

```nix
       { bob.fred = 3; } // { bob.jill =    4;        } = { bob.jill = 4; }
```

If we instead use `infuse`, we end up with both `bob.fred` and `bob.jill`:

```nix
infuse { bob.fred = 3; }    { bob.jill = _: 4;        } = { bob.fred = 3; bob.jill = 4; }
```

Unlike `lib.modules`, we can get back the "clobbering" behavior if we want it:

```nix
infuse { bob.fred = 3; }    { bob = _: { jill = 4; }; } = { bob.jill = 4; }
```

This allows to merge attrsets containing *structured* values which should
replace each other (or report an error) rather than getting mixed together.

### Lists

If you infuse a list to something, it acts like `lib.pipe`:

```nix
infuse { bob.fred = 3; } { bob.fred = [ (x: x+1) (x: x*x) ]; } = { bob.fred = 16; }
```

### Desugaring

That's everything there is to know about the fundamental `infuse-desugared`
operation, which is exported by this library.  The `infuse` function builds on
this, by first *desugaring* an infusion and then passing it to
`infuse-desugared`.  The fancy `__input`, `__output`, `__append`, and
`__prepend` attributes are just sugar -- the desugaring operation replaces them
with ordinary infusions.  If you don't need them you can use `infuse-desugared`
directly.

## Tutorial

See [TUTORIAL.md](TUTORIAL.md) for an introduction.

## Examples

For an example of what `infuse.nix` can do, see [amjoseph's overlays](example.md).
