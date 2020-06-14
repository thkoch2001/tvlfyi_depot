{ depot, pkgs, ... }:

let
  origArgs = depot.gerrit.gerritArgs;
in
pkgs.buildBazelPackage (origArgs // {
  name = "gerrit-tvl.jar";
  bazelTarget = "//plugins/tvl";

  fetchAttrs = origArgs.fetchAttrs // {
    sha256 = "1gkzxb8gsi5y3pnlkyjpb1sfkps1nqsv74cx0x3mxz5gjksw57pn";
    preBuild = origArgs.fetchAttrs.preBuild + ''
      cp -Rv ${./.} plugins/tvl
    '';
  };

  buildAttrs = origArgs.buildAttrs // {
    preConfigure = origArgs.buildAttrs.preConfigure + ''
      cp -Rv ${./.} plugins/tvl
    '';
    installPhase = ''
      cp bazel-bin/plugins/tvl/tvl.jar "$out"
    '';
  };
}) // { name = "gerrit-tvl"; }
