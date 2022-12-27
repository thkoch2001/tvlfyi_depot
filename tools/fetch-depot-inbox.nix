# Wrapper script that uses offlineimap to fetch the depot inbox from
# inbox.tvl.su.
#
# Run with the desired output directory as the only argument.
#
# Alternatively, users can browse the inbox on https://inbox.tvl.su
# and interact with public-inbox in any other supported way (IMAP,
# NNTP, git, etc.).
{ pkgs, depot, ... }:

let
  config = pkgs.writeText "offlineimaprc" ''
    [general]
    accounts = depot

    [Account depot]
    localrepository = Local
    remoterepository = Remote

    [Repository Local]
    type = Maildir
    # localfolders set by CLI

    [Repository Remote]
    type = IMAP
    ssl = yes
    sslcacertfile = ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    remotehost = inbox.tvl.su
    remoteuser = anonymous
    remotepass = anonymous
  '';
in
pkgs.writeShellScriptBin "fetch-depot-inbox" ''
  readonly MAILDIR=''${1}

  if [ -z "''${MAILDIR}" ]; then
    echo "[inbox] must specify target maildir as the first argument!" >&2
    exit 1
  fi

  if [ ! -d "''${MAILDIR}" ]; then
    echo "[inbox] specified maildir must exist and be a directory!" >&2
    exit 1
  fi

  echo "[inbox] Synchronising TVL depot inbox into ''${MAILDIR}"
  ${pkgs.offlineimap}/bin/offlineimap -c ${config} \
    -k "Repository_Local:localfolders=''${MAILDIR}"
''
