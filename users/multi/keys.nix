{ ... }:

let
  keys = {
    whitby = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOtKHxl87H1RSHU7xs37PcDej+EREdPRQKuaAUfgKvLR multi@in-addr.xyz";
    yubikey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCl9/WmXJcZ1dWAHQDgjJksG2vEkeSyG+b70dLk5bb5CdytsLsv8oJYPH+g2CXkaCAPc0oGfS92KRcYk7gwNinckbNWCq5Y4rG51L92yfujoXNDuddSDtPZT1PTvPTv0YmkD9/rE4SUFuPJ/bYPBqsW40BdK3f5EbISAiKmRuulZrEl053H0aRNmNbCbK4LNDgFYO78XkCmNEDAftGu5N1zY0RbU3YBw/oDzj/zWEXmliEGrpM9CrTkPbV0+OsTWpw5QXdkMUGg/YnNgtEJpDdnE5cIo1CGoUgm/jgVzvloEwCSv/2nOczGdDSE1z7YCSxNO2vSeioFiCJlQhFOKFV88GhlnOmL0YhGCBGakRFh8Ld61urdwGGbws/9z6nRZ5eiCqPw791kz0L0rhwdD9bkYjdJopkMx2rc50nRlPZVJM6Wl5oegGsZa20v9Kw5PJQsJNyPyVMGNKaRcTylu4lNaOeyP3pd7PjbDjmtRGSptDhPuaUjJrSTRizlWZ2PIODkd9b9n5PzquI/itCPNUdA4Ofe96Xm1oLae8psTWMnxt8hraQeIJZuiLJkdonI22BEA2voMciSLyFLGc1suo9HzMkmy3eJMZktKTjD9Nib9gOTqoNQevgcmu2oa1ZJdHA5vGi/obmT820/4+67HiJbcpIGgHp5wPtWfxmwM1RiZw== cardno:000608114525";
  };

  configs = {
    whitby = [ keys.whitby keys.yubikey ];
  };

in
  configs
