# this wraps import to override readFile and readDir to trace the files it touches
# technique inspired by lorri
let

  global = {
    import = global.scopedImport { };
    scopedImport = x: builtins.scopedImport (global // x);
    builtins = builtins // {
      inherit (global) import scopedImport;
      readFile = path: builtins.trace "depot-scan '${toString path}'" (builtins.readFile path);
      readDir = path: builtins.trace "depot-scan '${toString path}'" (builtins.readDir path);
    };
  };

in
global.import
