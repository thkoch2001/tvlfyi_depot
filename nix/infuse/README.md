# infuse.nix

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

See [TUTORIAL.md](TUTORIAL.md) for an introduction.
