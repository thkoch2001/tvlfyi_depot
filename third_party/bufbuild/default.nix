# buf.build is a Protobuf linter and breaking change detector.
# Several binaries are produced.
{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "buf";
  version = "v0.20.1";
  vendorSha256 = "1gg5c7aiqb4w1zxwsraxxpln33xkmkzlp1h69xgi9i08zvrfipqs";

  src = pkgs.fetchFromGitHub {
    owner = "bufbuild";
    repo = "buf";
    rev = "5e8bf4c800de911764ffdf8d2188b7f6f54476e4";
    sha256 = "1rni5swfnb4sbrd9rls4mc3902xhqrlsja96lfcdfjzx08g6kg20";
  };

  doCheck = false;

  # TODO(riking): postinstall produce shell completions for bash, fish, zsh
  # bin/buf bash-completion
  # bin/buf zsh-completion
  # # bin/buf manpages # not yet functional

  meta = with lib; {
    description = "Protobuf linter and breaking change detector";
    homepage = "https://buf.build/docs/introduction";
    license = licenses.asl20;
  };
}
