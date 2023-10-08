# Standard NixOS users for TVL machines, as well as configuration that
# should following along when they are added to a machine.
{ depot, pkgs, ... }:

{
  users = {
    users.tazjin = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
    };

    users.lukegb = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = depot.users.lukegb.keys.all;
    };

    users.grfn = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = [
        depot.users.grfn.keys.whitby
      ];
    };

    users.edef = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.edef.keys.all;
    };

    users.qyliss = {
      isNormalUser = true;
      description = "Alyssa Ross";
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.qyliss.keys.all;
    };

    users.eta = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.eta.keys.whitby;
    };

    users.cynthia = {
      isNormalUser = true; # I'm normal OwO :3
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.cynthia.keys.all;
    };

    users.firefly = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.firefly.keys.whitby;
    };

    users.sterni = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = depot.users.sterni.keys.all;
    };

    users.flokli = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = depot.users.flokli.keys.all;
    };
  };

  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    alacritty.terminfo
    foot.terminfo
    rxvt-unicode-unwrapped.terminfo
    kitty.terminfo
  ];
}
