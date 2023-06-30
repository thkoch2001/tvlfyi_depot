# https://github.com/josh-project/josh
{ depot, pkgs, ... }:

let
  # TODO(sterni): switch to pkgs.josh as soon as that commit is released
  rev = "c0a170a756dd5e63268673086218c0ce7bf18bdc";
  src = pkgs.fetchFromGitHub {
    owner = "josh-project";
    repo = "josh";
    inherit rev;
    hash = "sha256:0rsf65fq9xm3qj77ig3s4wmmgm50jhvwrknr839hipjj5lj4x1hp";
  };


  rust169 = depot.third_party.nixpkgs.rust-bin.stable."1.69.0".default;
  naersk = pkgs.callPackage depot.third_party.sources.naersk {
    rustc = rust169;
    cargo = rust169;
  };
in
naersk.buildPackage {
  inherit src;
  JOSH_VERSION = "git-${builtins.substring 0 8 rev}";

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkg-config
  ];

  dontStrip = true;
  cargoBuildOptions = x: x ++ [
    "-p"
    "josh-filter"
    "-p"
    "josh-proxy"
  ];

  overrideMain = x: {
    preBuild = x.preBuild or "" + ''
      echo 'debug = true' >> Cargo.toml
    '';

    nativeBuildInputs = (x.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/josh-proxy --prefix PATH : "${pkgs.git}/bin"
    '';
  };

  meta.ci.extraSteps = {
    # debug step to see if josh works now
    filteredPushTest = {
      label = ":thinking: does josh work now? :thinking:";
      alwaysRun = true;
      command = pkgs.writeShellScript "josh-filter-test" ''
        set -xe
        ${depot.third_party.josh}/bin/josh-filter ':/nix/nix-1p'
      '';
    };
  };
}
