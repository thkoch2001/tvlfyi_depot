{ pkgs ? (import ../../../. {}).third_party, ... }:

let

  hlib = pkgs.haskell.lib;

in

pkgs.haskellPackages.extend (self: super: {
  regex-tdfa-text = hlib.doJailbreak
    (hlib.appendPatch
      super.regex-tdfa-text
      ./regex-tdfa-text.patch
    );

  fullstop = hlib.dontCheck super.fullstop;

  chatter = hlib.doJailbreak
    (hlib.dontCheck (hlib.appendPatch super.chatter ./chatter.patch));
})
