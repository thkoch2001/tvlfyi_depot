{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "git-bug";
  version = "unstable-20200614";
  vendorSha256 = "1b8m9b05jbwx336fyv8sxcki00vpinyl95nyhygi7yj0as7x978x";

  nativeBuildInputs = [ pkgs.installShellFiles ];

  src = pkgs.fetchFromGitHub {
    owner = "MichaelMure";
    repo = "git-bug";
    rev = "6352d6aa2338f47cd8b60631dec5f4161d9d92ec";
    sha256 = "1cwsl3n6w8gfzx4j9sb5f7vdnxpnyhsq1lbf4f90ivmshjc2i8fw";
  };

  # git-bug builds 3 binaries (git-bug itself, misc and doc). The two
  # additional binaries are used to generate man pages and shell
  # completions. However, the generated things are already checked in.
  postBuild = ''
    rm $GOPATH/bin/misc $GOPATH/bin/doc
  '';

  postInstall = ''
    installManPage doc/man/*
    installShellCompletion --bash misc/bash_completion/git-bug
    installShellCompletion --fish misc/fish_completion/git-bug
    installShellCompletion --zsh misc/zsh_completion/git-bug
  '';

  meta = with lib; {
    description = "Distributed, offline-first bug tracker embedded in git";
    homepage = "https://github.com/MichaelMure/git-bug";
    license = licenses.gpl3;
  };
}
