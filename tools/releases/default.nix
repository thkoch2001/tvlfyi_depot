# Definitions for simple release mechanisms from depot.
{ depot, lib, pkgs, ... }:

let
  inherit (lib.strings) sanitizeDerivationName;
in
{
  # Use a josh filter to push a certain subset of canon to another git
  # repository.
  #
  # This expects, of course, that the remote repository has granted
  # push access to the CI SSH key.
  filteredGitPush = { filter, remote, ref ? "refs/heads/canon", force ? false }: {
    label = ":git: push '${filter}' to external git repository";
    branches = [ "canon" ];
    postBuild = true;

    command = pkgs.writeShellScript "${sanitizeDerivationName filter}-push" ''
      set -e
      echo 'Filtering depot through ${filter}'
      ${depot.third_party.josh}/bin/josh-filter '${filter}'

      echo 'Pushing filtered repository to ${remote}:${ref}'
      ${pkgs.git}/bin/git push ${lib.optionalString force "-f"} \
        '${remote}' FILTERED_HEAD:${ref}
    '';
  };
}
