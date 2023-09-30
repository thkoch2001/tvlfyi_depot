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
  vendorSha256 = "00w4jvcpgwh01ddhmlqcnyf4w9gh9bv1g12y9imbhba9cgmg20z5";

  meta = with pkgs.lib; {
    license = licenses.asl20;
    maintainers = [ maintainers.tazjin ];
  };
}
