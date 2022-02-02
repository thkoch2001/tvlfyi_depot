{ pkgs, ... }:

let
  thing = pkgs.runCommandNoCC "testtool"
    {
      VALUE = "incorrect";
    } ''
    echo "$VALUE" > $out
  '';
in
thing.overrideAttrs (_: {
  passthru.meta.ci.extraSteps.check = {
    needsOutput = true;
    alwaysRun = true;
    parentOverride = (parent: parent.overrideAttrs (_: {
      VALUE = "correct";
    }));

    command = pkgs.writeShellScript "check" ''
      cat result
    '';
  };
})
