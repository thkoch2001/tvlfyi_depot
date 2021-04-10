# Notmuch, but with support for https://dotti.me/
{ pkgs, ... }:

pkgs.notmuch.overrideAttrs(old: {
  doCheck = false;
  patches = [ ./dottime.patch ] ++ (if old ? patches then old.patches else []);
})
