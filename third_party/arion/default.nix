{ pkgs, ... }:

(import (pkgs.fetchFromGitHub {
  owner = "hercules-ci";
  repo = "arion";
  rev = "db6d4d7490dff363de60cebbece3ae9361e3ce43";
  sha256 = "0d8nqmc7fjshigax2g47ips262v8ml27x0ksq59kmprgb7ckzi5l";
}) { inherit pkgs; }).arion
