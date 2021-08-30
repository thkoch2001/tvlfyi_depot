{ depot, lib, pkgs, ... }:

let

  inherit (depot.users.sterni.nix.flow)
    cond
    ;

  inherit (depot.nix)
    yants
    ;

  inherit (depot.users.sterni.nix)
    string
    ;

  # A char is the atomic element of a nix string
  # which is essentially an array of arbitrary bytes
  # as long as they are not a NUL byte.
  #
  # A char is neither a byte nor a unicode codepoint!
  char = yants.restrict "char" (s: builtins.stringLength s == 1) yants.string;

  # integer representation of char
  charval = yants.restrict "charval" (i: i >= 1 && i < 256) yants.int;

  allChars = builtins.readFile ./all-chars.bin;

  # Originally I searched a list for this, but came to the
  # conclusion that this can never be fast enough in Nix.
  # We therefore use a solution similar to infinisil's.
  ordMap = builtins.listToAttrs
    (lib.imap1 (i: v: { name = v; value = i; })
      (string.toChars allChars));

  # Note on performance:
  # chr and ord have been benchmarked using the following cases:
  #
  #  builtins.map ord (lib.stringToCharacters allChars)
  #  builtins.map chr (builtins.genList (int.add 1) 255
  #
  # The findings are as follows:
  # 1. Searching through either strings using recursion is
  #    unbearably slow in Nix, leading to evaluation times
  #    of up to 3s for the following very small test case.
  #    This is why we use the trusty attribute set for ord.
  # 2. String indexing is much faster than list indexing which
  #    is why we use the former for chr.
  ord = c: ordMap."${c}";

  chr = i: string.charAt (i - 1) allChars;

  asciiAlpha = c:
    let
      v = ord c;
    in (v >= 65 && v <= 90)
    || (v >= 97 && v <= 122);

  asciiNum = c:
    let
      v = ord c;
    in v >= 48 && v <= 57;

  asciiAlphaNum = c: asciiAlpha c || asciiNum c;

in {
  inherit
    allChars
    char
    charval
    ord
    chr
    asciiAlpha
    asciiNum
    asciiAlphaNum
    ;

  # originally I generated a nix file containing a list of
  # characters, but infinisil uses a better way which I adapt
  # which is using builtins.readFile instead of import.
  __generateAllChars = pkgs.runCommandCC "generate-all-chars" {
    source = ''
      #include <stdio.h>

      int main(void) {
        for(int i = 1; i <= 0xff; i++) {
          putchar(i);
        }
      }
    '';
    passAsFile = [ "source" ];
  } ''
    $CC -o "$out" -x c "$sourcePath"
  '';
}
