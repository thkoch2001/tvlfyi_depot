{ depot, pkgs, lib, ... }:

let
  nixosTestDrv = pkgs.nixosTest {
    name = "tvix-daemon-vm-test";
    nodes.machine = { config, pkgs, ... }: {
      environment.systemPackages = [
        (pkgs.writers.writeBashBin "poke-daemon" ''
          NIX_REMOTE=unix:///nix/var/nix/daemon-socket/socket nix-instantiate -E '"''${/etc/nscd.conf}"'
        '')
      ];
      systemd.services.nix-daemon.serviceConfig.ExecStart = [
        ""
        "${depot.users.picnoir.tvix-daemon.tvix-daemon}/bin/tvix-daemon"
      ];

    };
    testScript = ''
      machine.wait_for_unit("multi-user.target")
      machine.succeed("poke-daemon")
    '';
  };
in nixosTestDrv // {
    # The test fails for now. TOREMOVE when we reach the stage where we
    # can add stuff to the store.
  meta.ci.skip = true;
}
