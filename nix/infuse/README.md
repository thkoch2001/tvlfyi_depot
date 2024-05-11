# infuse.nix

## What?

`infuse.nix` is a "deep" version of both .override and .overrideAttrs, which can
be used as a leaner untyped alternative to lib.modules.  It works well with
yants if you need runtime type-checking.

The semantics of infusion have a clear and simple inductive specification (see
default.nix) and it obeys basic algebraic laws (identity, associativity,
distributivity -- see tests/default.nix).

## Why?

Would you rather write this:

```nix
pkgs // {
  xrdp = (pkgs.xrdp.override {
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

## How?

The basic idea is that `infuse` acts like `lib.recursiveUpdate`, except that in
the second (right-hand) argument you must *mark* any subtrees where you want the
automatic merging to stop.  You mark those subtrees by changing them from
whatever value they were into a function which returns that value (and ignores
its argument).

In the following example, `fred` gets clobbered because we used `//`:

```nix
       { bob.fred = 3; } // { bob.jill =    4;        } == { bob.jill = 4; }
```

If we instead use `infuse`, we end up with both `bob.fred` and `bob.jill`,
because we have marked `bob.jill` by making its value a function (`_: 4`):

```nix
infuse { bob.fred = 3; }    { bob.jill = _: 4;        } == { bob.fred = 3; bob.jill = 4; }
```

Unlike `lib.modules`, we can get back the "clobbering" behavior if we want, by
marking an attribute higher up the tree:

```nix
infuse { bob.fred = 3; }    { bob = _: { jill = 4; }; } == { bob.jill = 4; }
```

This allows to merge attrsets containing *structured* values which should
replace each other (or report an error) rather than getting mixed together.

### Lists

If you infuse a list to something, it acts like `lib.pipe`:

```nix
infuse { bob.fred.x = 3; } { bob.fred = [ { x = x: x*x; } (fred: fred.x+1) ]; }) == { bob.fred = 10; }
```

### Desugaring

That's all there is to know about the `infuse-desugared`, which is the
fundamental operation for the infuse library.

The more-general `infuse` function -- which supports the fancy `__input`,
`__output`, `__append`, and `__prepend` attributes -- simply feeds its infusion
through a desugaring function which turns an infusion with these special
attribute into a *desugared infusion* which uses only functions and lists.  If
you don't need the fancy `__`-prefixed attributes you can call
`infuse-desugared` directly.

## Examples

For an example of what `infuse.nix` can do, see [amjoseph's overlays](example.md).
