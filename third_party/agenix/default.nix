{ pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "ryantm";
    repo = "agenix";
    rev = "52ea2f8c3231cc2b5302fa28c63588aacb77ea29";
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
