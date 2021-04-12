{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = pkgs.fetchFromGitHub {
    owner = "jonhoo";
    repo = "buzz";
    rev = "aede85850bd4b919128da6267c8cf151e7246a35";
    sha256 = "02z3cqqfaylasqyafv0fbzcfav0vby7nngh642gb4cxrm3jqkz2w";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs.pkgsBuildTarget; [
    llvmPackages.bintools
    llvmPackages.clang
    pkgconfig
  ];

  buildInputs = with pkgs; [
    dbus_libs
    glib
    openssl
    cairo
    pango
    atk
    gdk-pixbuf
    gtk3
    dbus-glib
    libappindicator-gtk3
    llvmPackages.libclang
    llvmPackages.llvm
  ];

  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang}/lib/libclang.so";
}
