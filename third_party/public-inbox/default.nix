{ pkgs, ... }:

pkgs.public-inbox.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ ./0001-feat-always-set-the-List-ID-header-even-in-watch.patch ];

  doCheck = false; # too slow, and nixpkgs already runs them
})
