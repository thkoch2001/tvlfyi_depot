with import <nixpkgs> { config.allowUnfree = true; };
mkShell {
  buildInputs = [
    awscli
    gnumake
    tarsnap
    certbot
  ];
}
