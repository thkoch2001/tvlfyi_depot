{ depot, lib, ... }:

let

  # TODO(sterni): encode

  inherit (depot.users.sterni.nix)
    char
    flow
    fun
    int
    string
    util
    ;

  /* (Internal) function to determine the amount
     bytes left in a UTF-8 byte sequence from the
     first byte.

     This function will throw if the given first
     byte is ill-formed, but will not detect all
     cases of ill-formed-ness.

     Based on table 3-6. from The Unicode Standard,
     Version 13.0, section 3.9.

     Type: integer -> integer
  */
  byteCount = i: flow.cond [
    [ (int.bitAnd i 128 == 0)   1 ]
    [ (int.bitAnd i 224 == 192) 2 ]
    [ (int.bitAnd i 240 == 224) 3 ]
    [ (int.bitAnd i 248 == 240) 4 ]
    [ true (builtins.throw "Ill-formed first byte ${int.toHex i}") ]
  ];

  /* (Internal) function to check if a given byte in
     an UTF-8 byte sequence is well-formed.

     Based on table 3-7. from The Unicode Standard,
     Version 13.0, section 3.9.

     Throws if the first byte is invalid.

     Type: integer -> integer -> (integer -> bool)
  */
  wellFormedByte =
    # first byte's integer value
    first:
    # byte position as an index starting with 0
    pos:
      let
        defaultRange = int.inRange 128 191;
      in
        # The first byte is either ASCII which requires no checks
        # or we automatically check it when we check the subsequent
        # bytes. The downside is that this may generate bad error
        # messages in very rare cases.
        if pos == 0
        then lib.const true
        else if pos > 1 # 3rd and 4th byte have only one validity rule
        then defaultRange
        else assert pos == 1; flow.switch first [
          [ (int.inRange 194 223) defaultRange          ] # C2..DF
          [ 224                   (int.inRange 160 191) ] # E0
          [ (int.inRange 225 236) defaultRange          ] # E1..EC
          [ 237                   (int.inRange 128 159) ] # ED
          [ (int.inRange 238 239) defaultRange          ] # EE..EF
          [ 240                   (int.inRange 144 191) ] # F0
          [ (int.inRange 241 243) defaultRange          ] # F1..F3
          [ 244                   (int.inRange 128 143) ] # F4
          [
            (fun.const true)
            (builtins.throw "Invalid first byte ${int.toHex first}")
          ]
        ];

  /* Iteration step for decoding an UTF-8 byte sequence.
     It decodes incrementally, i. e. it has to be fed
     one byte at a time and then returns either a
     new state or a final result.

     If the resulting attribute set contains the attribute
     result, it is finished and the decoded codepoint is
     contained in that attribute. In all other cases,
     pass the returned set to step again along with
     a new byte. The initial state to pass is the empty
     set.

     Extra attributes are always passed through, so you
     can pass extra state. Be sure not to use result,
     pos, code, first or count.

     This function will throw with a fairly detailed
     message if it encounters ill-formed bytes.

     The implementation is based on The Unicode Standard,
     Version 13.0, section 3.9, especially table 3-6.

     Type: { ... } -> string -> ({ result :: integer, ... } | { ... })

     Example: utf8.step {} "f"
     => { result = 102; }
  */
  step = { pos ? 0, code ? 0, ... }@args: byte:
    let
      value = char.ord byte;
      # first byte is context for well-formed-ness
      first = args.first or value;
      count = args.count or (byteCount first);
      newCode =
        if count == 1
        then int.bitAnd 127 first # ascii character
        else # multi byte UTF-8 sequence
          let
            # Calculate the bitmask for extracting the
            # codepoint data in the current byte.
            # If the codepoint is not ASCII, the bits
            # used for codepoint data differ depending
            # on the byte position and overall byte
            # count. The first byte always ignores
            # the (count + 1) most significant bits.
            # For all subsequent bytes, the 2 most
            # significant bits need to be ignored.
            # See also table 3-6.
            mask =
              if pos == 0
              then int.exp 2 (8 - (count + 1)) - 1
              else 63;
            # UTF-8 uses the 6 least significant bits in all
            # subsequent bytes after the first one. Therefore
            # We can determine the amount we need to shift
            # the current value by the amount of bytes left.
            offset = (count - (pos + 1)) * 6;
          in
            code + (int.bitShiftL (int.bitAnd mask value) offset);
      illFormedMsg =
        "Ill-formed byte ${int.toHex value} at position ${toString pos} in ${toString count} byte UTF-8 sequence";
    in
      if !(wellFormedByte first pos value) then builtins.throw illFormedMsg
      else if pos + 1 == count
      then (builtins.removeAttrs args [ # allow extra state being passed through
        "count"
        "code"
        "pos"
        "first"
      ]) // { result = newCode; }
      else (builtins.removeAttrs args [ "result" ]) // {
        inherit count first;
        code = newCode;
        pos  = pos + 1;
      };

  /* Decode an UTF-8 string into a list of codepoints.

     Throws if the string is ill-formed UTF-8.

     Type: string -> [ integer ]
  */
  # TODO(sterni): option to fallback to replacement char instead of failure
  decode = s:
    let
      stringLength = builtins.stringLength s;
      iterResult = builtins.genericClosure {
        startSet = [
          {
            key = "start";
            stringIndex = -1;
            state = {};
            codepoint = null;
          }
        ];
        operator = { state, stringIndex, ... }:
          let
            # updated values for current iteration step
            newIndex = stringIndex + 1;
            newState = step state (builtins.substring newIndex 1 s);
          in lib.optional (newIndex < stringLength) {
            # unique keys to make genericClosure happy
            key = toString newIndex;
            # carryover state for the next step
            stringIndex = newIndex;
            state = newState;
            # actual payload for later, steps with value null are filtered out
            codepoint = newState.result or null;
          };
      };
    in
    # extract all steps that yield a code point into a list
    builtins.map (v: v.codepoint) (
      builtins.filter (
        { codepoint, stringIndex, state, ... }:

        let
          # error message in case we are missing bytes at the end of input
          earlyEndMsg =
            if state ? count && state ? pos
            then "Missing ${toString (with state; count - pos)} bytes at end of input"
            else "Unexpected end of input";
        in

        # filter out all iteration steps without a codepoint value
        codepoint != null
          # if we are at the iteration step of a non-empty input string, throw
          # an error if no codepoint was returned, as it indicates an incomplete
          # UTF-8 sequence.
          || (stringLength > 0 && stringIndex == stringLength - 1 && throw earlyEndMsg)

      ) iterResult
    );

  /* Decodes an UTF-8 string, but doesn't throw on error.
     Instead it returns null.

     Type: string -> ( [ integer ] | null)
  */
  decodeSafe = s:
    let
      res = builtins.tryEval (decode s);
    in
      if res.success
      then res.value
      else null;

in {
  inherit
    decode
    decodeSafe
    step
    ;
}
