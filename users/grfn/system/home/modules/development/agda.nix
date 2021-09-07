{ config, lib, pkgs, ... }:

let
  agda-categories = with pkgs.agdaPackages; mkDerivation rec {
    pname = "agda-categories";
    version = "2128fab";
    src = pkgs.fetchFromGitHub {
      owner = "agda";
      repo = "agda-categories";
      rev = version;
      sha256 = "08mc20qaz9vp5rhi60rh8wvjkg5aby3bgwwdhfnxha1663qf1q24";
    };

    buildInputs = [ standard-library ];
  };

in

{
  imports = [
    ../lib/cloneRepo.nix
  ];

  home.packages = with pkgs; [
    (pkgs.agda.withPackages
      (p: with p; [
        p.standard-library

      ]))
  ];

  grfn.impure.clonedRepos = {
    agda-stdlib = {
      github = "agda/agda-stdlib";
      path = "code/agda-stdlib";
    };

    agda-categories = {
      github = "agda/agda-categories";
      path = "code/agda-categories";
    };

    categories-examples = {
      github = "agda/categories-examples";
      path = "code/categories-examples";
    };
  };

  home.file.".agda/defaults".text = ''
    standard-library
  '';

  home.file.".agda/libraries".text = ''
    /home/grfn/code/agda-stdlib/standard-library.agda-lib
    /home/grfn/code/agda-categories/agda-categories.agda-lib
  '';

}
