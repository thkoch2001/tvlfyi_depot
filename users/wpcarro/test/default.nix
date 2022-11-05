{ pkgs, ... }:

let
  dummy = pkgs.runCommand { } "dummy" "touch $out";
in
dummy.overrideAttrs (_: {
  meta.ci.extraSteps = {
    root = {
      command = "sleep 3 && exit 0";
    };
    a = {
      dependsOn = [ "depot.users.wpcarro.test.meta.ci.extraSteps.root" ];
      command = "sleep 3 && exit 0";
    };
    b = {
      dependsOn = [ "depot.users.wpcarro.test.meta.ci.extraSteps.root" ];
      command = "sleep 3 && exit 0";
    };
    c = {
      dependsOn = [ "depot.users.wpcarro.test.meta.ci.extraSteps.root" ];
      command = "sleep 3 && exit 0";
    };
    fin = {
      dependsOn = [
        "depot.users.wpcarro.test.meta.ci.extraSteps.a"
        "depot.users.wpcarro.test.meta.ci.extraSteps.b"
        "depot.users.wpcarro.test.meta.ci.extraSteps.c"
      ];
      command = "sleep 3 && exit 0";
    };
  };
})
