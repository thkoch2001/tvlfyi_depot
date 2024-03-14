{
  depot,
  pkgs,
  lib,
  ...
}:

pkgs.rustPlatform.buildRustPackage {
  name = "gio-list-apps";
  src = lib.cleanSource ./.;
  cargoLock.lockFile = ./Cargo.lock;
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [
    pkgs.gtk3
    depot.users.tazjin.emacs.emacs
  ];

  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp
    ln -s $out/lib/libgio_list_apps.so $out/share/emacs/site-lisp/gio-list-apps.so
  '';
}
