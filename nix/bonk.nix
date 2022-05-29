{depot, lib, ...}:

let
  # TODO: what to do about "symlink"? How to resolve?
  getDirIn = inPath: dir:
    let ls = builtins.readDir inPath;
    in if builtins.hasAttr dir ls
       then if ls.${dir} == "directory"
            then "${inPath}/${dir}"
            else throw "${inPath}/${dir} is not a directory, but a ${ls.${dir}}"
       else throw "directory \"${dir}\" does not exist in ${inPath}";

  getFileIn = inPath: file:
    let ls = builtins.readDir inPath;
    in if builtins.hasAttr file ls
       then if ls.${file} == "regular"
            then "${inPath}/${file}"
            else throw "${inPath}/${file} is not a file, but a ${ls.${file}}"
       else throw "file \"${file}\" does not exist in ${inPath}";

  targetToPath = startingPath: { packagePath, artifact }:
    let
      packageDirPath = builtins.foldl' (path: dir: getDirIn path dir) startingPath packagePath;
      packageArtifactFile = getFileIn packageDirPath "${artifact}.nix";
    in packageArtifactFile;


  test =
    targetToPath (toString ../.)
      {
        packagePath = [ "tools" "magrathea" ];
        artifact = "default";
      };

in test
