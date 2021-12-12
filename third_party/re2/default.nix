{ pkgs, ... }:

pkgs.re2.override { stdenv = pkgs.fullLlvm11Stdenv; }
