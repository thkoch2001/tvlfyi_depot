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
     byte is ill-formed.

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
     Version 13.0.

     Throws if the first byte is invalid.

     Type: integer -> integer -> integer -> bool
  */
  wellFormedByte =
    # first byte's integer value
    first:
    # byte position as an index starting with 0
    pos:
      let
        defaultRange = int.inRange 128 191;
      in
        if pos == 0 # first byte is checked for subsequent bytes automatically
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
      first = args.first or value;
      count = args.count or (byteCount value);
      newCode =
        if count == 1
        then int.bitAnd 127 first # ascii character
        else # multi byte UTF-8 sequence
          let
            offset = (count - (pos + 1)) * 6;
            mask =
              if pos == 0
              then int.exp 2 (8 - (count + 1)) - 1
              else 63;
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
  decode = s:
    let
      iter = { codes ? [], ... }@args: byte:
        let
          res = step args byte;
        in
          builtins.deepSeq res
            (if res ? result
            then res // {
              codes = codes ++ [ res.result ];
            }
            else res);
      iterResult =
        builtins.foldl' iter {} (string.toChars s);
      earlyEndMsg =
        if iterResult ? count && iterResult ? pos
        then "Missing ${toString (with iterResult; count - pos)} bytes at end of input"
        else "Unexpected end of input";
    in
      if iterResult ? result
      then iterResult.codes
      else builtins.throw earlyEndMsg;

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
