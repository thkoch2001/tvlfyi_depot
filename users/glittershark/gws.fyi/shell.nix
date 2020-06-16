with import <nixpkgs> {};
mkShell {
  buildInputs = [
    awscli
    gnumake
    letsencrypt
    tarsnap
  ];
}
