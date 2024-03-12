# NixOS module to configure the Estonian e-ID software.
{ pkgs, ... }:

{
  services.pcscd.enable = true;

  # Tell p11-kit to load/proxy opensc-pkcs11.so, providing all available slots
  # (PIN1 for authentication/decryption, PIN2 for signing).
  environment.etc."pkcs11/modules/opensc-pkcs11".text = ''
    module: ${pkgs.opensc}/lib/opensc-pkcs11.so
  '';

  # Configure Firefox (in case users set `programs.firefox.enable = true;`)
  programs.firefox = {
    # Allow a possibly installed "Web eID" extension to do native messaging with
    # the "web-eid-app" native component.
    # Users not using `programs.firefox.enable` can override their firefox
    # derivation, by setting `extraNativeMessagingHosts = [ pkgs.web-eid-app ]`.
    nativeMessagingHosts.packages = [ pkgs.web-eid-app ];
    # Configure Firefox to load smartcards via p11kit-proxy.
    # Users not using `programs.firefox.enable` can override their firefox
    # derivation, by setting
    # `extraPolicies.SecurityDevices.p11-kit-proxy "${pkgs.p11-kit}/lib/p11-kit-proxy.so"`.
    policies.SecurityDevices.p11-kit-proxy = "${pkgs.p11-kit}/lib/p11-kit-proxy.so";
  };

  # Chromium users need a symlink to their (slightly different) .json file
  # in the native messaging hosts' manifest file location.
  environment.etc."chromium/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";
  environment.etc."opt/chrome/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";

  environment.systemPackages = with pkgs; [
    libdigidocpp.bin # provides digidoc-tool(1)
    qdigidoc

    # Wrapper script to tell to Chrome/Chromium to use p11-kit-proxy to load
    # security devices, so they can be used for TLS client auth.
    # Each user needs to run this themselves, it does not work on a system level
    # due to a bug in Chromium:
    #
    # https://bugs.chromium.org/p/chromium/issues/detail?id=16387
    #
    # Firefox users can just set
    # extraPolicies.SecurityDevices.p11-kit-proxy "${pkgs.p11-kit}/lib/p11-kit-proxy.so";
    # when overriding the firefox derivation.
    (pkgs.writeShellScriptBin "setup-browser-eid" ''
      NSSDB="''${HOME}/.pki/nssdb"
      mkdir -p ''${NSSDB}

      ${pkgs.nssTools}/bin/modutil -force -dbdir sql:$NSSDB -add p11-kit-proxy \
        -libfile ${pkgs.p11-kit}/lib/p11-kit-proxy.so
    '')
  ];
}
