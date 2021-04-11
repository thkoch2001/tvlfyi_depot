{ config, lib, pkgs, ... }:

let

  nixpkgs-gnuradio = import (pkgs.fetchFromGitHub {
    owner = "doronbehar";
    repo = "nixpkgs";
    rev = "712561aa5f10bfe6112a1726a912585612a70d1f";
    sha256 = "04yqflbwjcfl9vlplphpj82csqqz9k6m3nj1ybhwgmsc4by7vivl";
  }) {};

in

{
  home.packages = with pkgs; [
    rtl-sdr
    nixpkgs-gnuradio.gnuradio
    nixpkgs-gnuradio.gnuradio.plugins.osmosdr
    nixpkgs-gnuradio.gqrx
  ];
}
