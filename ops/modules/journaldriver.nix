# Configures journaldriver to forward to the tvl-fyi GCP project from
# TVL machines.
{ config, depot, ... }:

{
  imports = [
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  age.secrets.journaldriver = depot.ops.secrets."journaldriver.age";

  services.journaldriver = {
    enable = true;
    googleCloudProject = "tvl-fyi";
    logStream = config.networking.hostName;
    applicationCredentials = "journaldriver.json";
  };

  # Override the systemd service defined in the nixpkgs module to pass
  # the LoadCredential= option.
  systemd.services.journaldriver.serviceConfig.LoadCredential =
    "journaldriver.json:/run/agenix/journaldriver";
}
