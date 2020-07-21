{ depot, ... }:

with depot.third_party;

buildGoModule rec {
  pname = "NoiseTorch";
  version = "0.5.2-beta";

  src = fetchFromGitHub {
    owner = "lawl";
    repo = "NoiseTorch";
    rev = version;
    sha256 = "1q0gfpqczlpybxcjjkiybcy6yc0gnrq8x27r0mpg4pvgwy7mps47";
  };

  patches = [./no-git-version.patch];

  preBuild = ''
    mkdir -p librnnoise_ladspa/bin/ladspa
    ln -s ${nsfv}/lib/ladspa/librnnoise_ladspa.so librnnoise_ladspa/bin/ladspa
    go generate
    rm -r scripts
  '';

  propagatedBuildInputs = [ nsfv ];
  deleteVendor = true;
  vendorSha256 = "1lr376gcvvhmpvf0ydrri6xngz938xiw277wx7gpbawyy4ndc101";
}
