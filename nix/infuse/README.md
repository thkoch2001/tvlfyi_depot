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

I'm currently using it for my own personal overlays, and also for a much larger
project.  I'm never going back.  The larger project isn't ready yet, but
`infuse.nix` is.

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
infuse { bob.fred = 3; }    { bob = _: { jill = 4; }; } = { bob.fred = 3; bob.jill = 4; }  # but you can still clobber if you want to!
```

This allows to merge attrsets containing *structured* values which should
replace each other (or report an error) rather than getting mixed together.

## Tutorial

See [TUTORIAL.md](TUTORIAL.md) for an introduction.
