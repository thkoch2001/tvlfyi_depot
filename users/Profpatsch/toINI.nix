{ lib, ... }:
let
  /* Generate an INI-style config file from an attrset
   * specifying the global section (no header), and a
   * list of sections which contain name/value pairs.
   *
   * generators.toINI {} {
   *   globalSection = [
   *     { name = "someGlobalKey"; value = "hi"; }
   *   ];
   *   sections = [
   *     { name = "foo"; value = [
   *         { name = "hi"; value = "${pkgs.hello}"; }
   *         { name = "ciao"; value = "bar"; }
   *       ];
   *     }
   *     { name = "baz";
   *       value = [ { name = "also, integers"; value = 42; } ];
   *     }
   *   ];
   * }
   *
   *> someGlobalKey=hi
   *>
   *> [foo]
   *> hi=/nix/store/y93qql1p5ggfnaqjjqhxcw0vqw95rlz0-hello-2.10
   *> ciao=bar
   *>
   *> [baz]
   *> also, integers=42
   *>
   *
   * The mk* configuration attributes can generically change
   * the way sections and key-value strings are generated.
   *
   * Order of the sections and of keys is preserved,
   * duplicate keys are allowed.
   */
  toINI = {
    # apply transformations (e.g. escapes) to section names
    mkSectionName ? (name: lib.strings.escape [ "[" "]" ] name),
    # format a setting line from key and value
    mkKeyValue    ? lib.generators.mkKeyValueDefault {} "=",
  }: { globalSection, sections }:
    let
        mkSection = sectName: sectValues: ''
          [${mkSectionName sectName}]
        '' + toKeyValue { inherit mkKeyValue; } sectValues;
        # map input to ini sections
        mkSections = lib.strings.concatMapStringsSep "\n"
          ({name, value}: mkSection name value)
          sections;
        mkGlobalSection =
          if globalSection == []
          then ""
          else toKeyValue { inherit mkKeyValue; } globalSection
            + "\n";
    in
    mkGlobalSection
    + mkSections;

  /* Generate a name-value-style config file from a list.
   *
   * mkKeyValue is the same as in toINI.
   */
  toKeyValue = {
    mkKeyValue ? lib.generators.mkKeyValueDefault {} "=",
  }:
  let mkLine = k: v: mkKeyValue k v + "\n";
      mkLines = k: v: [ (mkLine k v) ];
  in nameValues: lib.strings.concatStrings (lib.concatLists (map ({name, value}: mkLines name value) nameValues));

in toINI
