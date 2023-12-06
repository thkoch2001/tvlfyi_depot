# Finally, a good FUSE FS implementation over S3.
# https://github.com/yandex-cloud/geesefs

{ pkgs, ... }:

pkgs.buildGoModule rec {
  pname = "geesefs";
  version = "0.38.3";

  src = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "geesefs";
    rev = "v${version}";
    sha256 = "0kf0368hnards619azz8xw7cp7fm806v0aszmgq24qs9ax45dv6m";
  };

  subPackages = [ "." ];
  buildInputs = [ pkgs.fuse ];
  vendorSha256 = "sha256-5QPx6mNJLbhqTF6EF/ZK8CVOnLcM0wpbCwDyd9mWhAM=";

  meta = with pkgs.lib; {
    license = licenses.asl20;
    maintainers = [ maintainers.tazjin ];
  };
}
