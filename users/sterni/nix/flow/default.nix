{ depot, ... }:

let

  inherit (depot.nix)
    yants
    ;

  # we must avoid evaluating any of the sublists
  # as they may contain conditions that throw
  condition = yants.restrict "condition"
    (ls: builtins.length ls == 2)
    (yants.list yants.any);

  /* cond :: [ [ bool any ] ] -> any
   *
   * Like the common lisp macro: takes a list
   * of two elemented lists whose first element
   * is a boolean. The second element of the
   * first list that has true as its first
   * element is returned.
   *
   * Example:
   *
   * cond [
   *   [ (builtins.isString true) 12 ]
   *   [ (3 == 2) 13 ]
   *   [ true 42 ]
   * ]
   *
   * => 42
   */
  cond = conds:
    if builtins.length conds == 0
    then builtins.throw "cond: exhausted all conditions"
    else
      let
        c = condition (builtins.head conds);
      in
        if builtins.head c
        then builtins.elemAt c 1
        else cond (builtins.tail conds);

  # TODO(sterni): condf or magic
  # like <nixpkgs/pkgs/build-support/coq/extra-lib.nix>

  match = val: matcher: matcher."${val}";

in {
  inherit
    cond
    match
    ;
}
