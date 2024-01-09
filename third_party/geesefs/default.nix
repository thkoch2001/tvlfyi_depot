# Finally, a good FUSE FS implementation over S3.
# https://github.com/yandex-cloud/geesefs

{ pkgs, ... }:

pkgs.buildGoModule rec {
  pname = "geesefs";
  version = "0.40.0";

  src = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "geesefs";
    rev = "v${version}";
    sha256 = "0ibvzp83rwzpd4fx4wmk46j570jw8c3hbd9yw55dqwjzk6ay0aq2";
  };

  subPackages = [ "." ];
  buildInputs = [ pkgs.fuse ];
  vendorSha256 = "11i7cmnlxi00d0csgpv8drfcw0aqshwc4hfs0jw7zwafdhnlyy0j";

  meta = with pkgs.lib; {
    license = licenses.asl20;
    maintainers = [ maintainers.tazjin ];
  };
}
