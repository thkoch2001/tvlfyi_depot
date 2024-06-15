{ depot, pkgs, lib, ... }:

depot.tvix.crates.workspaceMembers.tvixbolt.build.overrideAttrs {
  passthru.serve = pkgs.writeShellScript "serve-tvixbolt" ''
    ${lib.getExe pkgs.simple-http-server} \
        --index \
        --nocache \
        "$@" \
        ${depot.tvix.tvixbolt}
  '';
}
