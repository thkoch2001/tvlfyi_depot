# NixOS module to configure the Estonian e-ID software.
{ pkgs, ... }:

let
  # Wrapper script to tell to Chrome/Chromium to use p11-kit-proxy to load
  # security devices.
  # Each user needs to run this themselves, it does not work on a system level
  # due to a bug in Chromium:
  #
  # https://bugs.chromium.org/p/chromium/issues/detail?id=16387
  #
  setup-browser-eid = pkgs.writeShellScriptBin "setup-browser-eid" ''
    NSSDB="''${HOME}/.pki/nssdb"
    mkdir -p ''${NSSDB}

    ${pkgs.nssTools}/bin/modutil -force -dbdir sql:$NSSDB -add p11-kit-proxy \
      -libfile ${pkgs.p11-kit}/lib/p11-kit-proxy.so
  '';
in
{
  services.pcscd.enable = true;

  # Tell p11-kit to load onepin-opensc-pkcs11.so
  environment.etc."pkcs11/modules/onepin-opensc-pkcs11".text = ''
    module: ${pkgs.opensc}/lib/onepin-opensc-pkcs11.so
  '';

  environment.systemPackages = with pkgs; [
    qdigidoc
    setup-browser-eid
  ];
}
