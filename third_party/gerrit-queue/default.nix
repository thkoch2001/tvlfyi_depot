{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "gerrit-queue";
  version = "master";
  vendorSha256 = "0hivr4yn9aa1vk7z1h1nwg75hzqnsaxypi1wwxdy1l1hnm5k8hhi";

  src = pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "gerrit-queue";
    rev = "c67b3ba7ea769cd747ea2f43ee6d12943d599ae0";
    hash = "sha256:1x0g6fd5hymf6a8wxj1b1xi4x1hmwpnx4f2cdidgvsyd77v902c1";
  };

  # Add TVL patches to bend gerrit-queue functionality to our will.
  patches = [
    # Use 'Autosubmit' label instead of Gerrit hashtags.
    ./0001-gerrit-Use-a-Gerrit-label-instead-of-hashtag-for-aut.patch
  ];

  # gerrit-queue embeds static assets which need to be generated
  nativeBuildInputs = [ pkgs.statik ];
  preBuild = ''
    statik -f
  '';

  meta = with lib; {
    description = "Gerrit submit bot";
    homepage = "https://github.com/tweag/gerrit-queue";
    license = licenses.asl20;
  };
}
