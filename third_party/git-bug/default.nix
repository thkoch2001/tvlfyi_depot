{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "git-bug";
  version = "unstable-20200614";
  vendorSha256 = "1lmcs1b0rr2xfajmz205pjp94v8ih1qpj69za06wbp24r5nc2cjg";
  doCheck = false;

  nativeBuildInputs = [ pkgs.installShellFiles ];

  src = pkgs.fetchFromGitHub {
    owner = "lukegb";
    repo = "git-bug";
    rev = "8243cc989f9dff1546978ba70dc93ab14c232033";
    sha256 = "0y30bm8imhn2rcrg1s0bswdd60bwdzym60mwrk7fnmgjgscnrss3";
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
