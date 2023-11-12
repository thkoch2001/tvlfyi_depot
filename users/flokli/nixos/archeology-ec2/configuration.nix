{ depot, pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
    ../profiles/archeology.nix
  ];

  systemd.timers.parse-bucket-logs = {
    wantedBy = [ "multi-user.target" ];
    timerConfig.OnCalendar = "*-*-* 03:00:00 UTC";
  };

  systemd.services.parse-bucket-logs = {
    path = [ depot.users.flokli.archeology.parse-bucket-logs ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = (pkgs.writers.writePython3 "parse-bucket-logs-continuously"
        {
          libraries = [ pkgs.python3Packages.boto3 ];
        } ./parse-bucket-logs-continuously.py);
      DynamicUser = "yes";
      StateDirectory = "parse-bucket-logs";
    };
  };

  environment.systemPackages = [
    depot.users.flokli.archeology.parse-bucket-logs
  ];

  networking.hostName = "archeology-ec2";

  system.stateVersion = "23.05"; # Did you read the comment?
}

