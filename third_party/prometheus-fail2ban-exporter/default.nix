{ pkgs, ... }:

pkgs.writeShellScriptBin "prometheus-fail2ban-exporter" ''
  set -eo pipefail

  exec "${pkgs.python3.withPackages (p: [
    p.prometheus_client
  ])}/bin/python" "${pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/jangrewe/prometheus-fail2ban-exporter/11066950b47bb2dbef96ea8544f76e46ed829e81/fail2ban-exporter.py";
    sha256 = "049lsvw1nj65bbvp8ygyz3743ayzdawrbjixaxmpm03qbrcfmwc4";
  }}"
''
