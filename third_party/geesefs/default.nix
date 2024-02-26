# Finally, a good FUSE FS implementation over S3.
# https://github.com/yandex-cloud/geesefs

{ pkgs, ... }:

pkgs.buildGoModule rec {
  pname = "geesefs";
  version = "0.40.1";

  src = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "geesefs";
    rev = "v${version}";
    hash = "sha256:0ig8h17z8n5j8qb7k2jyh40vv77zazhnz8bxdam9xihxksj8mizp";
  };

  subPackages = [ "." ];
  buildInputs = [ pkgs.fuse ];
  vendorHash = "sha256:11i7cmnlxi00d0csgpv8drfcw0aqshwc4hfs0jw7zwafdhnlyy0j";

  meta = with pkgs.lib; {
    license = licenses.asl20;
    maintainers = [ maintainers.tazjin ];
  };
}
