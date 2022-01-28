{ depot
, pkgs
, lib
, ...
}:
let
  bins =
    depot.nix.getBins pkgs.stow [ "stow" ]
      // depot.nix.getBins pkgs.coreutils [ "mkdir" "ln" "printenv" "rm" ]
      // depot.nix.getBins pkgs.xe [ "xe" ]
      // depot.nix.getBins pkgs.lr [ "lr" ]
      // depot.nix.getBins pkgs.nix [ "nix-store" ];
  # run stow to populate the target directory with the given stow package, read from stowDir.
  # Bear in mind that `stowDirOriginPath` should always be semantically bound to the given `stowDir`, otherwise stow might become rather confused.
  runStow =
    { # “stow package” to stow (see manpage)
      stowPackage
    , # “target directory” to stow in (see manpage)
      targetDir
    , # The “stow directory” (see manpage), containing “stow packages” (see manpage)
      stowDir
    , # representative directory for the stowDir in the file system, against which stow will create relative links.
      # ATTN: this is always overwritten with the contents of `stowDir`! You shouldn’t re-use the same `stowDirOriginPath` for different `stowDir`s, otherwise there might be surprises.
      stowDirOriginPath
    ,
    }:
    depot.nix.writeExecline
      "stow-${ stowPackage }"
      { }
      [
        # first, create a temporary stow directory to use as source
        # (stow will use it to determine the origin of files)
        "if"
        [ bins.mkdir "-p" stowDirOriginPath ]
        # remove old symlinks
        "if"
        [ "pipeline" [ bins.lr "-0" "-t" "depth == 1 && type == l" stowDirOriginPath ] bins.xe "-0" bins.rm ]
        # create an indirect gc root so our config is not cleaned under our asses by a garbage collect
        "if"
        [ bins.nix-store "--realise" "--indirect" "--add-root" "${ stowDirOriginPath }/.nix-stowdir-gc-root" stowDir ]
        # populate with new stow targets
        "if"
        [ "elglob" "-w0" "stowPackages" "${ stowDir }/*" bins.ln "--force" "-st" stowDirOriginPath "$stowPackages" ]
        # stow always looks for $HOME/.stowrc to read more arguments
        "export"
        "HOME"
        "/homeless-shelter"
        bins.stow
        # always run restow for now; this does more stat but will remove stale links
        "--restow"
        "--dir"
        stowDirOriginPath
        "--target"
        targetDir
        stowPackage
      ];
  # create a stow dir from a list of drv paths and a stow package name.
  makeStowDir =
    ( with depot.nix.yants; defun [ ( list ( struct { originalDir = drv; stowPackage = string; } ) ) drv ] )
      (
        dirs:
        depot.nix.runExecline
          "make-stow-dir"
          {
            stdin =
              lib.pipe
                dirs
                [ ( map depot.users.Profpatsch.netencode.gen.dwim ) depot.users.Profpatsch.netstring.toNetstringList ];
          }
          [
            "importas"
            "out"
            "out"
            "if"
            [ bins.mkdir "-p" "$out" ]
            "forstdin"
            "-d"
            ""
            "-o"
            "0"
            "line"
            "pipeline"
            [ depot.users.Profpatsch.execline.print-one-env "line" ]
            depot.users.Profpatsch.netencode.record-splice-env
            "importas"
            "-ui"
            "originalDir"
            "originalDir"
            "importas"
            "-ui"
            "stowPackage"
            "stowPackage"
            bins.ln
            "-sT"
            "$originalDir"
            "\${out}/\${stowPackage}"
          ]
      );
in
# TODO: temp setup
lib.pipe
  { }
  [
    (
      _:
      makeStowDir
        [
          {
            stowPackage = "scripts";
            originalDir =
              pkgs.linkFarm "scripts-farm" [ { name = "scripts/ytextr"; path = depot.users.Profpatsch.ytextr; } ];
          }
        ]
    )
    (
      d:
      runStow
        {
          stowDir = d;
          stowPackage = "scripts";
          targetDir = "/home/philip";
          stowDirOriginPath = "/home/philip/.local/share/nix-home/stow-origin";
        }
    )
  ]
