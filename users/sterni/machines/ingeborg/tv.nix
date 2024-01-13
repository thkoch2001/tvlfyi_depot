{ pkgs, ... }:

{
  config = {
    # TODO(sterni): smb or nfs may be a faster alternative?
    services.openssh.allowSFTP = true;

    users.users.tv = {
      group = "users";
      isNormalUser = true;
    };
  };
}
