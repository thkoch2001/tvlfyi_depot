{ ... }:

{
  /* For a list of length n that consists of lists of length m,
     return a list of length m containing lists of length n
     so that

         builtins.elemAt (builtins.elemAt orig a) b
         == builtins.elemAt (builtins.elemAt transposed b) a

     Essentially, if you think of the nested list as an array with two
     dimensions, the two index axes are swapped.

     The length of the inner lists m is determined based on the first element
     and assumed to be used for all other lists. Malformed input data may
     cause the function to crash or lose data.

     Type: <n>[ <m>[ ] ] -> <m>[ <n>[ ] ]
  */
  transpose = list:
    let
      innerLength = builtins.length (builtins.head list);
      outerLength = builtins.length list;
    in
    builtins.genList
      (inner: builtins.genList
        (outer: builtins.elemAt (builtins.elemAt list outer) inner)
        outerLength)
      innerLength;
}
