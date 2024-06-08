{ depot, pkgs, ... }:

{
  imports = [
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  age.secrets.geesefs-tazjins-files.file = depot.users.tazjin.secrets."geesefs-tazjins-files.age";
  programs.fuse.userAllowOther = true;

  systemd.services.geesefs = {
    description = "geesefs @ tazjins-files";
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.fuse ];

    serviceConfig = {
      # TODO: can't get fusermount to work for non-root users (e.g. DynamicUser) here, why?

      Restart = "always";
      LoadCredential = "geesefs-tazjins-files:/run/agenix/geesefs-tazjins-files";
      StateDirectory = "geesefs";
      ExecStartPre = "/run/wrappers/bin/umount -a -t fuse.geesefs";
    };

    script = ''
      set -u # bail out if systemd is misconfigured ...
      set -x

      mkdir -p $STATE_DIRECTORY/tazjins-files $STATE_DIRECTORY/cache

      ${pkgs.geesefs}/bin/geesefs \
        -f -o allow_other \
        --cache $STATE_DIRECTORY/cache \
        --shared-config $CREDENTIALS_DIRECTORY/geesefs-tazjins-files \
        tazjins-files $STATE_DIRECTORY/tazjins-files
    '';
  };
}
