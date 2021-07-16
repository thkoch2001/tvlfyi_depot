{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = pkgs.fetchFromGitHub {
    owner = "jonhoo";
    repo = "buzz";
    rev = "02479643ed1b0325050245dbb3b70411b8cffb7a";
    sha256 = "1spklfv02qlinlail5rmhh1c4926gyrkr2ydd9g6z919rxkl0ywk";
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
