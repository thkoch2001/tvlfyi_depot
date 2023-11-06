with import <nixpkgs> { config.allowUnfree = true; };
mkShell {
  buildInputs = [
    awscli
    gnumake
    letsencrypt
    tarsnap
  ];
}
