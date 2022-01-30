{ config, lib, pkgs, ... }:

let
  deb = ./kolide.deb;

  kolide = pkgs.runCommand "kolide-data"
    {
      buildInputs = [ pkgs.binutils-unwrapped ];
    } ''
    cp ${deb} ./kolide.deb
    ar x kolide.deb
    mkdir result
    tar xzf data.tar.gz -C result
    patchelf \
      --set-interpreter ${pkgs.glibc}/lib/ld-linux-x86-64.so.2 \
      --set-rpath "${lib.makeLibraryPath (with pkgs; [
        zlib
      ])}" \
      result/usr/local/kolide-k2/bin/osqueryd
    mv result $out
  '';

in
{
  systemd.services."launcher.kolide-k2" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" "syslog.service" ];
    description = "The Kolide Launcher";
    serviceConfig = {
      ExecStart = ''
        ${kolide}/usr/local/kolide-k2/bin/launcher \
          -config \
          ${pkgs.writeText "launcher.flags" ''
            with_initial_runner
            control
            autoupdate
            root_directory /var/lib/kolide
            osqueryd_path ${kolide}/usr/local/kolide-k2/bin/osqueryd
            enroll_secret_path ${kolide}/etc/kolide-k2/secret
            control_hostname k2control.kolide.com
            update_channel stable
            transport jsonrpc
            hostname k2device.kolide.com
          ''}
      '';
      StateDirectory = "kolide";
      Restart = "on-failure";
      RestartSec = 3;
    };
  };
}
