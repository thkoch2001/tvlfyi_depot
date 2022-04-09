# NixOS module to configure the Estonian e-ID software.
{ pkgs, ... }:

let
  # Wrapper script to add EID support to Chrome. Each user needs to
  # run this themselves, it does not work on a system level.
  setup-browser-eid = pkgs.writeShellScriptBin "setup-browser-eid" ''
    NSSDB="''${HOME}/.pki/nssdb"
    mkdir -p ''${NSSDB}

    ${pkgs.nssTools}/bin/modutil -force -dbdir sql:$NSSDB -add opensc-pkcs11 \
      -libfile ${pkgs.opensc}/lib/onepin-opensc-pkcs11.so -mechanisms FRIENDLY
  '';
in
{
  services.pcscd.enable = true;

  environment.systemPackages = with pkgs; [
    qdigidoc
    setup-browser-eid
  ];
}
