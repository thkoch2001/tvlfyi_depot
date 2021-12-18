{ depot, ... }:

let

  inherit (depot.nix)
    yants
    ;

  inherit (depot.users.sterni.nix)
    fun
    ;

  # we must avoid evaluating any of the sublists
  # as they may contain conditions that throw
  condition = yants.restrict "condition"
    (ls: builtins.length ls == 2)
    (yants.list yants.any);

  /* Like the common lisp macro: takes a list
    of two elemented lists whose first element
    is a boolean. The second element of the
    first list that has true as its first
    element is returned.

    Type: [ [ bool a ] ] -> a

    Example:

    cond [
    [ (builtins.isString true) 12 ]
    [ (3 == 2) 13 ]
    [ true 42 ]
    ]

    => 42
  */
  cond = conds: switch true conds;

  /* Generic pattern match-ish construct for nix.
    Takes a bunch of lists which are of length
    two and checks the first element for either
    a predicate or a value. The second value of
    the first list which either has a value equal
    to or a function that evaluates to true for
    the given value.

    Type: a -> [ [ (function | a) b ] ] -> b

    Example:

    switch "foo" [
    [ "smol" "SMOL!!!" ]
    [ (x: builtins.stringLength x <= 3) "smol-ish" ]
    [ (fun.const true) "not smol" ]
    ]

    => "smol-ish"
  */
  switch = x: conds:
    if builtins.length conds == 0
    then builtins.throw "exhausted all conditions"
    else
      let
        c = condition (builtins.head conds);
        s = builtins.head c;
        b =
          if builtins.isFunction s
          then s x
          else x == s;
      in
      if b
      then builtins.elemAt c 1
      else switch x (builtins.tail conds);



in
{
  inherit
    cond
    switch
    ;
}
