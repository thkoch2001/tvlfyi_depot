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
      script = pkgs.writeShellScript "no-config" ''
        echo 'no config'
      '';
    };

    no-label = {
      alwaysRun = true;
      script = pkgs.writeShellScript "no-label" ''
        echo 'no label'
      '';
    };

    gated = {
      alwaysRun = true;
      label = "gated extra step";
      gated = "Do you want to run this?";
      script = pkgs.writeShellScript "gated-check" ''
        echo 'ran the gated extra step'
      '';
    };

    gated-post = {
      alwaysRun = true;
      label = "gated post step";
      gated = "Do you want to run this?";
      # postCheck = true;
      script = pkgs.writeShellScript "gated-check" ''
        echo 'ran the gated post step'
      '';
    };

    run-output = {
      label = "run output";
      needsOutput = true;
      script = pkgs.writeShellScript "run-output" ''
        result/bin/mg
      '';
    };

    run-output-post = {
      label = "run output post";
      needsOutput = true;
      # postCheck = true;
      script = pkgs.wariteShellScript "run-output-post" ''
        result/bin/mg
      '';
    };
  };
}
