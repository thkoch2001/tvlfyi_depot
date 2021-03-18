# Lieer is a small tool to synchronise a Gmail account with a local
# maildir.
{ pkgs, ... }:

let
  inherit (pkgs) fetchFromGitHub python3Packages;
in python3Packages.buildPythonApplication rec {
  name = "lieer-${version}";
  version = "1.3";

  src = fetchFromGitHub {
    owner = "gauteh";
    repo = "lieer";
    rev = "v${version}";
    sha256 = "12sl7d381l1gjaam419xc8gxmsprxf0hgksz1f974qmmijvr02bh";
  };

  patches = [
    ./send_scope.patch
  ];

  propagatedBuildInputs = with python3Packages; [
    notmuch
    oauth2client
    google-api-python-client
    tqdm
  ];
}
