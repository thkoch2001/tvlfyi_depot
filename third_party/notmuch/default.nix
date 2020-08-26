{ pkgs, ... }:

pkgs.originals.notmuch.overrideAttrs(old: {
  doCheck = false;
  patches = [ ./dottime.patch ] ++ (if old ? patches then old.patches else []);

  # TODO(tazjin): Fix the build and re-enable CI.
  meta.ci = false;
})
