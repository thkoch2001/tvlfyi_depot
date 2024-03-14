{ depot, lib, ... }:

let

  inherit (lib) id;

  # Simple function composition,
  # application is right to left.
  rl = f1: f2: (x: f1 (f2 x));

  # Compose a list of functions,
  # application is right to left.
  rls = fs: builtins.foldl' (fOut: f: lr f fOut) id fs;

  # Simple function composition,
  # application is left to right.
  lr = f1: f2: (x: f2 (f1 x));

  # Compose a list of functions,
  # application is left to right
  lrs = x: fs: builtins.foldl' (v: f: f v) x fs;

  # Warning: cursed function
  #
  # Check if a function has an attribute
  # set pattern with an ellipsis as its argument.
  #
  # s/o to puck for discovering that you could use
  # builtins.toXML to introspect functions more than
  # you should be able to in Nix.
  hasEllipsis =
    f:
    builtins.isFunction f && builtins.match ".*<attrspat ellipsis=\"1\">.*" (builtins.toXML f) != null;

  /*
    Return the number of arguments the given function accepts or 0 if the value
    is not a function.

    Example:

      argCount argCount
      => 1

      argCount builtins.add
      => 2

      argCount pkgs.stdenv.mkDerivation
      => 1
  */
  argCount =
    f:
    let
      # N.B. since we are only interested if the result of calling is a function
      # as opposed to a normal value or evaluation failure, we never need to
      # check success, as value will be false (i.e. not a function) in the
      # failure case.
      called = builtins.tryEval (f (builtins.throw "You should never see this error message"));
    in
    if !(builtins.isFunction f || builtins.isFunction (f.__functor or null)) then
      0
    else
      1 + argCount called.value;

  /*
    Call a given function with a given list of arguments.

    Example:

      apply builtins.sub [ 20 10 ]
      => 10
  */
  apply = f: args: builtins.foldl' (f: x: f x) f args;

  # TODO(sterni): think of a better name for unapply
  /*
    Collect n arguments into a list and pass them to the given function.
    Allows calling a function that expects a list by feeding it the list
    elements individually as function arguments - the limitation is
    that the list must be of constant length.

    This is mainly useful for functions that wrap other, arbitrary functions
    in conjunction with argCount and apply, since lists of arguments are
    easier to deal with usually.

    Example:

      (unapply 3 lib.id) 1 2 3
      => [ 1 2 3 ]

      (unapply 5 lib.reverse) 1 2 null 4 5
      => [ 5 4 null 2 1 ]

      # unapply and apply compose the identity relation together

      unapply (argCount f) (apply f)
      # is equivalent to f (if the function has a constant number of arguments)

      (unapply 2 (apply builtins.sub)) 20 10
      => 10
  */
  unapply =
    let
      unapply' =
        acc: n: f: x:
        if n == 1 then f (acc ++ [ x ]) else unapply' (acc ++ [ x ]) (n - 1) f;
    in
    unapply' [ ];

  /*
    Optimize a tail recursive Nix function by intercepting the recursive
    function application and expressing it in terms of builtins.genericClosure
    instead. The main benefit of this optimization is that even a naively
    written recursive algorithm won't overflow the stack.

    For this to work the following things prerequisites are necessary:

    - The passed function needs to be a fix point for its self reference,
      i. e. the argument to tailCallOpt needs to be of the form
      `self: # function body that uses self to call itself`.
      This is because tailCallOpt needs to manipulate the call to self
      which otherwise wouldn't be possible due to Nix's lexical scoping.

    - The passed function may only call itself as a tail call, all other
      forms of recursions will fail evaluation.

    This function was mainly written to prove that builtins.genericClosure
    can be used to express any (tail) recursive algorithm. It can be used
    to avoid stack overflows for deeply recursive, but naively written
    functions (in the context of Nix this mainly means using recursion
    instead of (ab)using more performant and less limited builtins).
    A better alternative to using this function is probably translating
    the algorithm to builtins.genericClosure manually. Also note that
    using tailCallOpt doesn't mean that the stack won't ever overflow:
    Data structures, especially lazy ones, can still cause all the
    available stack space to be consumed.

    The optimization also only concerns avoiding stack overflows,
    tailCallOpt will make functions slower if anything.

    Type: (F -> F) -> F where F is any tail recursive function.

    Example:

    let
      label' = self: acc: n:
        if n == 0
        then "This is " + acc + "cursed."
        else self (acc + "very ") (n - 1);

      # Equivalent to a naive recursive implementation in Nix
      label = (lib.fix label') "";

      labelOpt = (tailCallOpt label') "";
    in

    label 5
    => "This is very very very very very cursed."

    labelOpt 5
    => "This is very very very very very cursed."

    label 10000
    => error: stack overflow (possible infinite recursion)

    labelOpt 10000
    => "This is very very very very very very very very very…
  */
  tailCallOpt =
    f:
    let
      argc = argCount (lib.fix f);

      # This function simulates being f for f's self reference. Instead of
      # recursing, it will just return the arguments received as a specially
      # tagged set, so the recursion step can be performed later.
      fakef = unapply argc (args: {
        __tailCall = true;
        inherit args;
      });
      # Pass fakef to f so that it'll be called instead of recursing, ensuring
      # only one recursion step is performed at a time.
      encodedf = f fakef;

      opt =
        args:
        let
          steps = builtins.genericClosure {
            # This is how we encode a (tail) call: A set with final == false
            # and the list of arguments to pass to be found in args.
            startSet = [
              {
                key = 0;
                final = false;
                inherit args;
              }
            ];

            operator =
              { key, final, ... }@state:
              let
                # Plumbing to make genericClosure happy
                newId = {
                  key = key + 1;
                };

                # Perform recursion step
                call = apply encodedf state.args;

                # If call encodes a new call, return the new encoded call,
                # otherwise signal that we're done.
                newState =
                  if builtins.isAttrs call && call.__tailCall or false then
                    newId
                    // {
                      final = false;
                      inherit (call) args;
                    }
                  else
                    newId
                    // {
                      final = true;
                      value = call;
                    };
              in

              if final then
                [ ] # end condition for genericClosure
              else
                [ newState ];
          };
        in
        # The returned list contains intermediate steps we ignore.
        (builtins.head (builtins.filter (x: x.final) steps)).value;
    in
    unapply argc opt;
in

{
  inherit (lib) fix flip const;

  inherit
    id
    rl
    rls
    lr
    lrs
    hasEllipsis
    argCount
    tailCallOpt
    apply
    unapply
    ;
}
