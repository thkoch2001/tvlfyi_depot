let s = "foo ${builtins.substring 33 100 (baseNameOf "${./evalstore-okay-context.nix}")} bar";
in
  if s != "foo evalstore-okay-context.nix bar"
  then abort "context not discarded"
  else builtins.unsafeDiscardStringContext s

