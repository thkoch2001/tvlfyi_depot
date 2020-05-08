{ pkgs, ... }:

(pkgs.originals.abseil-cpp.overrideAttrs(_: {
  version = "20200220-0232c87f";

  src = pkgs.fetchFromGitHub {
    owner = "abseil";
    repo = "abseil-cpp";
    rev = "0232c87f21c26718aa3eb2d86678070f3b498a4e";
    sha256 = "1shay44sg0glz1f2jbx7dyvrrqmnx07q7azwraigyj61f7b9ccyx";
  };
}))
