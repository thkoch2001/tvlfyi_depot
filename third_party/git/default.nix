# git with custom patches. This is also used by cgit via
# `pkgs.srcOnly`.
{ pkgs, ... }:

pkgs.git.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [
    ./0001-feat-third_party-git-date-add-dottime-format.patch
  ];
})
