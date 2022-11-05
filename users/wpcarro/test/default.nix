{ pkgs, ... }:

let
  dummy = pkgs.runCommand "dummy" { } "touch $out";
in
dummy.overrideAttrs (_: {
  meta.ci.extraSteps = {
    foo = {
      group.steps = [
        { command = "echo 1"; }
        { command = "echo 2"; }
        { command = "echo 3"; }
      ];
    };
    bar = {
      group.steps = [
        { command = "echo 4"; }
        { command = "echo 5"; }
        { command = "echo 6"; }
      ];
    };
  };
})
