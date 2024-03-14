# Configures journaldriver to forward to the tvl-fyi GCP project from
# TVL machines.
{
  config,
  depot,
  lib,
  pkgs,
  ...
}:

{
  imports = [ (depot.third_party.agenix.src + "/modules/age.nix") ];

  age.secrets.journaldriver.file = depot.ops.secrets."journaldriver.age";

  services.journaldriver = {
    enable = true;
    googleCloudProject = "tvl-fyi";
    logStream = config.networking.hostName;
  };

  # Override the systemd service defined in the nixpkgs module to use
  # the credentials provided by agenix.
  systemd.services.journaldriver = {
    serviceConfig = {
      LoadCredential = "journaldriver.json:/run/agenix/journaldriver";
      ExecStart = lib.mkForce "${pkgs.coreutils}/bin/env GOOGLE_APPLICATION_CREDENTIALS=\"\${CREDENTIALS_DIRECTORY}/journaldriver.json\" ${depot.ops.journaldriver}/bin/journaldriver";
    };
  };
}
