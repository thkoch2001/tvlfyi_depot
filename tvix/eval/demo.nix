#
# This demonstrates that tvix can eval nixpkgs's stdenv, with only
# two exceptions, marked below:
#
#  [1] We aren't doing `allowedRequisites` properly
#  [2] We map all store paths to fake names.
#
# Since the fake names are not the hash of a derivation, it is
# entirely possible that this name-faking is concealing bugs.
# However the fact that none of nixpkgs' assertions (which remain in
# force) have triggered makes it somewhat unlikely that this is the
# case.
#
let
  nixpkgs = import <nixpkgs> { };
  lib = nixpkgs.lib;
  sanitize = drv:
    if drv == null then null else
    builtins.mapAttrs
      (k: v:
        # have to blank the type attr of a derivation in order to
        # get toPretty to print the derivation rather than the
        # drvPath
        if k == "type" then null
        else if k == "drvAttrs" then sanitize v

        # [1] not handled correctly yet
        else if k == "allowedRequisites" then null

        # stringify what we can
        else if lib.strings.isCoercibleToString v
        then builtins.toString v

        else if lib.strings.hasPrefix "__" k then null

        # and drop the rest
        else "")
      drv;

  # [2]
  # tvix currently reports all outPaths as
  #   /nix/store/00000000000000000000000000000000-mock
  # and all drvPaths as
  #   /nix/store/00000000000000000000000000000000-mock.drv
  # so we rewrite cppnix's output accordingly
  mock = drv:
    lib.attrsets.mapAttrsRecursive
      (_: v:
        if v == null then null else
        let split = builtins.split "/nix/store/[0-9a-zA-Z.-]*" v;
        in lib.concatStrings
          (map
            (e:
              if e == null then ""
              else if lib.isList e then "/nix/store/00000000000000000000000000000000-mock"
              else e)
            split))
      drv;

in
lib.generators.toPretty { } (mock (sanitize nixpkgs.stdenv))
