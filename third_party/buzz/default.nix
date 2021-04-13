{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = pkgs.fetchFromGitHub {
    owner = "jonhoo";
    repo = "buzz";
    rev = "aede85850bd4b919128da6267c8cf151e7246a35";
    sha256 = "02z3cqqfaylasqyafv0fbzcfav0vby7nngh642gb4cxrm3jqkz2w";
    fetchSubmodules = true;
  };

  buildInputs = with pkgs; [
    pkgconfig
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
    llvmPackages.llvm
    llvmPackages.bintools
    llvmPackages.clang
    llvmPackages.libclang
  ];

  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib/libclang.so";
}
