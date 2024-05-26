{
  # Creates a powerset from a list. Used for trying all combinations of the crate's features.
  powerset = xs:
    let
      addElement = set: element:
        set ++ map (e: [ element ] ++ e) set;
      result = builtins.foldl' addElement [ [ ] ] xs;
    in
    result;
}
