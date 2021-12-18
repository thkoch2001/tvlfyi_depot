{ pkgs, ... }:

let
  commit = "52ea2f8c3231cc2b5302fa28c63588aacb77ea29";
  src = builtins.fetchTarball {
    url = "https://github.com/ryantm/agenix/archive/${commit}.tar.gz";
    sha256 = "1sqgbriwmvxcmqp0zbk7873psk9g60a53fgrr9p0jafki5zzgvdx";
  };
  agenix = import src {
    inherit pkgs;
  };
in
{
  inherit src;
  cli = agenix.agenix;
}
