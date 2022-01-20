# magrathea helps you build planets
#
# it is a tool for working with monorepos in the style of tvl's depot
{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "magrathea";
  src = ./.;
  dontInstall = true;

  nativeBuildInputs = [ pkgs.chicken ];
  buildInputs = with pkgs.chickenPackages.chickenEggs; [
    matchable
    srfi-13
  ];

  propagatedBuildInputs = [ pkgs.git ];

  buildPhase = ''
    mkdir -p $out/bin
    csc -o $out/bin/mg -static ${./mg.scm}
  '';

  passthru.meta.ci.extraSteps = {
    no-config = {
      command = pkgs.writeShellScript "no-config" ''
        echo 'no config'
      '';
    };

    no-label = {
      alwaysRun = true;
      command = pkgs.writeShellScript "no-label" ''
        echo 'no label'
      '';
    };

    with-prompt = {
      alwaysRun = true;
      label = "gated extra step";
      prompt = "Do you want to run this?";
      command = pkgs.writeShellScript "gated-check" ''
        echo 'ran the gated extra step'
      '';
    };

    run-output = {
      label = "run output";
      needsOutput = true;
      command = pkgs.writeShellScript "run-output" ''
        result/bin/mg
      '';
    };

    run-canon-only = {
      command = pkgs.writeShellScript "no-config" ''
        echo 'no config'
      '';
      branches = ["refs/heads/canon" "refs/heads/doesntexist"];
    };

    run-output-post = {
      label = "run output post build";
      needsOutput = true;
      postBuild = true;
      command = pkgs.writeShellScript "run-output-post" ''
        result/bin/mg
      '';
    };
  };
}
