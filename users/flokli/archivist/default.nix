{ depot, pkgs, ... }:
depot.nix.readTree.drvTargets {
  shell = pkgs.mkShell {
    name = "archivist-shell";
    packages = with pkgs; [ awscli2 ];

    AWS_PROFILE = "archivist";
    AWS_CONFIG_FILE = pkgs.writeText "aws-config" ''
      [sso-session nixos]
      sso_region = eu-north-1
      sso_start_url = https://nixos.awsapps.com/start
      sso_registration_scopes = sso:account:access

      [profile "archivist"]
      sso_session = nixos
      sso_account_id = 286553126452
      sso_role_name = AWSAdministratorAccess

      [profile "archeologist"]
      sso_session = nixos
      sso_account_id = 080433136561
      sso_role_name = archeologist
    '';
  };
}
