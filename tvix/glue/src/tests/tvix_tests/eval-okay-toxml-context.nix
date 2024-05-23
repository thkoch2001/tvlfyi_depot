# builtins.toXML retains context.

builtins.getContext (builtins.toXML {
  inherit (derivation {
    name = "test";
    builder = "/bin/sh";
    system = builtins.currentSystem;
  }) drvPath;
}) 
