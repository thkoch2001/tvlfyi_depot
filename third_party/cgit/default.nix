{ depot, lib, pkgs, ... }:

let inherit (pkgs) stdenv gzip bzip2 xz luajit zlib autoconf openssl pkgconfig;
in stdenv.mkDerivation rec {
  pname = "cgit";
  version = "master";
  src = ./.;

  nativeBuildInputs = [ autoconf pkgconfig ];
  buildInputs = [ openssl zlib luajit ];

  enableParallelBuilding = true;

  postPatch = ''
    sed -e 's|"gzip"|"${gzip}/bin/gzip"|' \
        -e 's|"bzip2"|"${bzip2.bin}/bin/bzip2"|' \
        -e 's|"xz"|"${xz.bin}/bin/xz"|' \
        -i ui-snapshot.c
  '';

  # Give cgit the git source tree including depot patches. Note that
  # the version expected by cgit should be kept in sync with the
  # version available in nixpkgs.
  #
  # TODO(tazjin): Add an assert for this somewhere so we notice it on
  # channel bumps.
  preBuild = ''
    rm -rf git # remove submodule dir ...
    cp -r --no-preserve=ownership,mode ${pkgs.srcOnly depot.third_party.git} git
    makeFlagsArray+=(prefix="$out" CGIT_SCRIPT_PATH="$out/cgit/")
    cat tvl-extra.css >> cgit.css
  '';

  meta = {
    homepage = "https://git.zx2c4.com/cgit/about/";
    repositories.git = "git://git.zx2c4.com/cgit";
    description = "Web frontend for git repositories";
    license = lib.licenses.gpl2;
    platforms = lib.platforms.linux;
    maintainers = with lib.maintainers; [ bjornfor ];
  };
}
