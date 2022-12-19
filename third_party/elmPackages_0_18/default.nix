# Backports Elm packages for Elm 0.18 from an older channel of
# nixpkgs.
#
# Elm 0.19 changed the language & package ecosystem completely,
# essentially requiring a partial rewrite of all Elm apps. However,
# //fun/gemma uses Elm 0.18 and I don't have time to rewrite it.
#
# Since the ABIs of current glibc and the pinned version have diverged
# too much, we need to build //fun/gemma completely based on a historical
# nixpkgs version.

{ pkgs, ... }:

(import
  (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "14f9ee66e63077539252f8b4550049381a082518";
    sha256 = "1wn7nmb1cqfk2j91l3rwc6yhimfkzxprb8wknw5wi57yhq9m6lv1";
  })
{ })
