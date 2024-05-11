#
# infuse.nix: a "deep" version of both .override and .overrideAttrs; can be
# used as a leaner untyped alternative to lib.modules; works well with yants.
#
# The canonical sources for this project, none of which require javascript, are:
#
#   (primary) https://code.tvl.fyi/tree/nix/infuse/default.nix
#   (backup)  https://git.sr.ht/~amjoseph/infuse.nix
#
#                               ** VENDOR ME! **
#
# This file is self-contained and has no dependencies other than <nixpkgs/lib>;
# you should copy it into your own project.
#

#
# The following is a specification, not an introduction.  See README.md for more
# details or TUTORIAL.md for an introduction.
#

#
# A *desugared infusion* is defined inductively as:
#
# - a function or
# - a list of desugared infusions or
# - an attrset whose values are desugared infusions
#
# The result of *infusing* a target with a desugared infusion is defined
# inductively based on the type of the infusion:
#
# - for a function: the function applied to the target
# - for an empty list: the target
# - for a non-empty list: the target infused with the first element of the list,
#   infused with the remainder of the list.
# - for an attrset:
#   - if the target is not an attrset: {} infused with the infusion
#   - if is the target is an attrset: the target updated (//) with an attrset
#     formed by infusing each attribute of the infusion upon the target
#     attrvalue having the same name if one exists, or upon `infuse.missing`.
#
# A (sugared) *infusion* is defined the same way as a desugared infusion, except
# that the values of attributes whose names begin with "__" may be any Nix
# value.  No attrset within an infusion may (currently) contain both
# "__"-prefixed attributes and non-"__"-prefixed attributes.
#
# To *desugar* an infusion, apply the `desugar` function to each attrset
# containing attributes having names which begin with "__".  The result of
# desugaring these attribute sets is a list containing one element for each
# attribute in the sugar-map; the value of the element is the sugar-map
# attrvalue applied to the infusion attrvalue of the same name, or [] if no such
# infusion attrvalue exists.  The order of the elements within this list is
# currently fixed, and customizing the sugar-map is not supported.
#
# The sugar-map currently has these attributes, processed in this order:
#
# __init     # assign a new attrvalue if none existed; otherwise `throw`
# __default  # assign a new attrvalue only if none already existed
# __prepend  # prepend a string, list, or function
# __input    # invoke .override
# __output   # invoke .overrideAttrs
# __append   # append a string, list, or function
# __infuse   # `infuse target { __apply = f; }` is the same as `infuse target f`
#
# The result of infusing a target with a infusion is the result of infusing the
# target with the desugared infusion.
#


{ lib ? import <nixpkgs/lib> { }
, ...
}:
let
  # This function exploits the bizzarre properties of Nix function equality to
  # implement "gensym":
  #
  #  https://code.tvl.fyi/tree/tvix/docs/src/value-pointer-equality.md
  #
  # The argument `ignored` can be any Nix value.
  #
  # This should be upstreamed into nixpkgs as lib.trivial.gensym,
  # and perhaps become builtins.gensym at some far-future date
  gensym = ignored: {
    inner = _: {
      inherit ignored;
      touch_me_and_die = throw "you tried to force gensym, fool";
    };
  };

  missing = gensym "missing-value-marker";
in

let

  #
  # Generally:
  #
  # - Functions with names ending in `-desugared` deal with infusions that have
  #   been desugared (i.e. already fed through `desugar`)
  #
  # - Functions with a prime after them (after', before', etc) take an
  #   additional `path` argument which is the attrpath at which they are being
  #   applied.  The `path` argument is used only for error reporting, so O(n^2)
  #   list-concatenations are okay; they won't be forced unless an error is
  #   encountered.  These primed functions are not exported.
  #
  inherit (builtins)
    typeOf length head;
  inherit (lib)
    concatStringsSep flip mapAttrs isFunction isAttrs isDerivation
    any all
    pipe zipAttrsWith isList isString id flatten filter hasPrefix
    attrNames;

  #
  # infuse-desugared :: Attrs -> Desugared-Infusion -> Attrs
  #
  infuse-desugared = target: infusion: builtins.deepSeq infusion (infuse-desugared' [ ] target infusion);

  infuse-desugared' =
    path: # attrpath relative to top-level call; only for error reporting
    target: # target attrset to be infused with the infusion
    infusion: # infusion to infuse upon the target attrset

    # fast path
    if infusion == { }
    then target

    else if isFunction infusion
    then infusion target

    else if isList infusion
    then infuse-desugared' path (pipeline-desugared' path (flatten infusion)) target

    # infusing to a derivation target
    else if isDerivation target
    then throw "infuse: at path \"${concatStringsSep "." path}\": attempted to infuse to subattributes of a derivation (did you forget to use desugar?)"

    # infusing to a non-derivation target
    else target //
      flip mapAttrs infusion
        (k: infusedval:
        let
          # used only for reporting errors; laziness ensures we don't pay
          # the obnoxious cost of this ++ operation unless something goes
          # wrong.
          path' = path ++ [ k ];
          fail = msg: throw "infuse: at path \"${concatStringsSep "." path'}\": ${msg}";
        in
        if isFunction infusedval then
        # new value is a function, so pass the old value (if any) to it
          infusedval (target.${k} or missing)
        else if !(isAttrs infusedval) || isDerivation infusedval then
          fail "leaf attributes of infusions must be functions; found a ${typeOf infusedval}"
        else
          infuse-desugared' path' (target.${k} or { }) infusedval);

  #
  # pipeline :: (List Infusion) -> Infusion
  #
  #   infuse target (pipeline list)
  #
  # is equivalent to:
  #
  #   lib.pipe target (map (lib.flip infuse) list
  #
  # ... but faster, especially if you reuse the infusion multiple times, since
  # deepSeq stops at functions but does not stop at attrsets.  See below; this
  # happens for every attrpath that takes the "recursive case -> fast path"
  # branch.
  #
  pipeline = infusions:
    let result = pipeline-desugared' [ ] (flatten infusions);
    in builtins.deepSeq result result;

  # pipeline-desugared' assumes that lib.flatten has been applied to infusions already
  pipeline-desugared' = path: infusions:

    # fast-path cases
    if infusions == [ ] then id
    else if (length infusions == 1) then head infusions

    # base case: no more attrsets, so use lib.pipe
    else if all isFunction infusions
    then old: pipe old infusions

    # recursive case: recurse over attributes
    else
      flip zipAttrsWith infusions
        (name: infusions':

          let infusions = flatten infusions'; in

          # fast path: no infusion at this attrpath is a function
          if all isAttrs infusions
          then pipeline-desugared' (path ++ [ name ]) infusions

          # TODO(amjoseph): in theory we could do better here by looking for
          # adjacent sequences of two or isAttrs, and collapse those separately
          # before resorting to lib.pipe.

          # slow path: resort to lib.pipe
          else
            flip pipe
              (flip map infusions
                (infusion:
                  if isFunction infusion
                  then infusion
                  else targetval: infuse-desugared' path targetval infusion)));

  # It's currently unclear how __-prefixed attribues should behave when either
  # their argument or the target attribute is a self-recursive functor.  To
  # preserve the flexibility to decide later, we currently `throw` when we
  # encounter these (most Nix functors ignore their self-recursive argument).
  forbid-fixpoint-functors = f:
    if !(f?__functor)
    then f
    else f.__functor (throw "infusing self-recursive functors to special (hasPrefix \"__\") attributes is not yet supported");

  throw-error =
    { path ? null
    , func
    , msg
    }:
    throw "infuse.input: at path ${concatStringsSep "." path}: ${msg}";

  defaultify = default: f: old:
    if old == missing
    then default
    else f old;

  __init = path: infusion:
    throw "not yet implemented; TODO(amjoseph)";

  __default = path: infusion:
    throw "not yet implemented; TODO(amjoseph)";

  __infuse = path: infusion:
    throw "not yet implemented; TODO(amjoseph)";

  __input = path: infusion:
    if infusion == missing then [ ]
    else if isAttrs infusion
    then __input path (previousArgs: infuse-desugared' path previousArgs infusion)
    else if isFunction infusion
    then old:
      if old?override then old.override infusion
      else if isFunction old then arg: old (infusion arg)
      else
        throw-error {
          inherit path;
          func = "input";
          msg = "attempted to infuse a function to target.__input but !(isFunction target)";
        }
    else
      throw-error {
        inherit path;
        func = "input";
        msg = "infused an unsupported type to __input: ${typeOf infusion}";
      };

  __output = path: infusion:
    if infusion == missing then [ ]
    else if isAttrs infusion
    then target: __output path (_: previousAttrs: infuse-desugared' path previousAttrs infusion) target
    else if isFunction infusion
    then target:
      if isFunction target then arg: infusion (target arg)
      else if isDerivation target && target?overrideAttrs
      then
        target.overrideAttrs
          (final: prev:
            let applied = infusion final; in
            if !(isFunction applied)
            then
              throw-error
                {
                  inherit path;
                  func = "output";
                  msg = "when infusing to drv.__output you must pass a *two*-argument curried function (i.e. `__output = finalAttrs: previousAttrs: ...`)";
                } else applied prev)
      else
        throw-error {
          inherit path;
          func = "input";
          msg = "attempted to infuse to __output of an unsupported type: ${typeOf target}";
        }
    else
      throw-error {
        inherit path;
        func = "output";
        msg = "applied to unsupported type: ${typeOf infusion}";
      };

  __prepend = path: infusion:
    if infusion == missing then [ ]
    else if isString infusion then
      defaultify infusion (string: assert isString string; infusion + string)
    else if isList infusion then
      defaultify infusion (list: assert isList list; infusion ++ list)
    else if isAttrs infusion then
      __prepend path (previousArgs: infuse-desugared' path previousArgs infusion)
    else
      throw-error {
        inherit path;
        func = "prepend";
        msg = "applied to unsupported type: ${typeOf infusion}";
      };

  __append = path: infusion:
    if infusion == missing then [ ]
    else if isString infusion then
      defaultify infusion (string: assert isString string; string + infusion)
    else if isList infusion then
      defaultify infusion (list: assert isList list; list ++ infusion)
    else if isAttrs infusion then
      __append path (previousArgs: infuse-desugared' path previousArgs infusion)
    else
      throw-error {
        inherit path;
        func = "append";
        msg = "applied to unsupported type: ${typeOf infusion}";
      };

  sugars = {
    inherit __prepend __append __input __output;
  };

  desugar = infusion:
    let result = desugar' [ ] infusion;
    in builtins.deepSeq result result;

  desugar' = path: infusion:
    if isFunction infusion then
      infusion
    else if (isList infusion) then
      pipeline-desugared' path (map (desugar' path) infusion)
    else if !(isAttrs infusion) then
      throw-error
        {
          inherit path;
          func = "desugar";
          msg = "invalid type ${typeOf infusion}";
        }
    else if !(any (hasPrefix "__") (attrNames infusion)) then
      mapAttrs (k: v: desugar' (path ++ [ k ]) v) infusion
    else if !(all (hasPrefix "__") (attrNames infusion)) then
      throw-error
        {
          inherit path;
          func = "desugar";
          msg = "mixing special (hasPrefix \"__\") and non-special attributes at the same path level is not (yet) supported";
        }
    else
      lib.pipe infusion [
        (lib.mapAttrs (name: val: if isFunction val || isAttrs val then desugar' (path ++ [ name ]) val else val))
        (lib.mapAttrs (name: val: sugars.${name} (path ++ [ name ]) val))
        (desugared: [
          (desugared.__init or [ ])
          (desugared.__default or [ ])
          (desugared.__prepend or [ ])
          (desugared.__input or [ ])
          (desugared.__output or [ ])
          (desugared.__append or [ ])
          (desugared.__infuse or [ ])
        ])
        flatten
        (pipeline-desugared' path)
      ];

in
{

  v1 = (rec {
    toplevel = {
      inherit infuse-desugared desugar;
      infuse = toplevel;
      __functor = self: target: infusion: infuse-desugared target (desugar infusion);
    };
  }).toplevel;

}
