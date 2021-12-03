{ pkgs, ... }:

let
  commit = "52ea2f8c3231cc2b5302fa28c63588aacb77ea29";
  src = builtins.fetchTarball {
    url = "https://github.com/ryantm/agenix/archive/${commit}.tar.gz";
    # replace this with an actual hash
    sha256 = "1sqgbriwmvxcmqp0zbk7873psk9g60a53fgrr9p0jafki5zzgvdx";
  };
in {
  inherit src;
  cli = import src {
    inherit pkgs;
  };
}
