{ pkgs, ... }:
{
  # programs.mbsync.enable = true;
  programs.lieer.enable = true;
  programs.notmuch.enable = true;
  services.lieer.enable = true;
  programs.msmtp.enable = true;

  home.packages = with pkgs; [
    mu
    msmtp
  ];

  accounts.email.maildirBasePath = "mail";
  accounts.email.accounts =
    let
      mkAccount = params@{ passEntry, ... }: {
        realName = "Griffin Smith";
        passwordCommand = "pass ${passEntry}";

        flavor = "gmail.com";

        imapnotify = {
          enable = true;
          boxes = [ "Inbox" ];
        };

        gpg = {
          key = "0F11A989879E8BBBFDC1E23644EF5B5E861C09A7";
          signByDefault = true;
        };

        # mbsync.enable = true;
        notmuch.enable = true;
        lieer = {
          enable = true;
          sync.enable = true;
        };
        msmtp.enable = true;
      } // builtins.removeAttrs params ["passEntry"];
    in {
      work = mkAccount {
        primary = true;
        address = "griffin@urbint.com";
        aliases = [ "grfn@urbint.com" ];
        passEntry = "urbint/msmtp-app-password";
      };

      personal = mkAccount {
        address = "root@gws.fyi";
        passEntry = "root-gws-msmtp";
      };
    };
}
