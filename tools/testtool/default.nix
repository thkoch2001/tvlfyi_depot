{ pkgs, ... }:

let
  thing = pkgs.runCommandNoCC "testtool"
    {
      VALUE = "incorrect";
    } ''
    echo "$VALUE" > $out
  '';
in
thing // {
  meta.ci.extraSteps.check = {
    needsOutput = true;
    parentOverride = (parent: parent.overrideAttrs (_: {
      VALUE = "correct";
    }));

    command = pkgs.writeShellScript "check" ''
      cat result
    '';
  };
}
