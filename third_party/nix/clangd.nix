# Create a clangd wrapper script that can be used with this project.
# The default Nix wrapper only works with C projects, not C++
# projects.
#
# The CPATH construction logic is lifted from the original wrapper
# script.

pkgs:

pkgs.writeShellScriptBin "nix-clangd" ''
  buildcpath() {
    local path
    while (( $# )); do
      case $1 in
          -isystem)
              shift
              path=$path''${path:+':'}$1
      esac
      shift
    done
    echo $path
  }

  export CPATH=''${CPATH}''${CPATH:+':'}:$(buildcpath ''${NIX_CFLAGS_COMPILE})
  export CPATH=${pkgs.glibc.dev}/include''${CPATH:+':'}''${CPATH}
  export CPLUS_INCLUDE_PATH=${pkgs.llvmPackages.libcxx}/include/c++/v1:''${CPATH}

  # TODO(tazjin): Configurable commands directory?
  exec -a clangd ${pkgs.llvmPackages.clang-unwrapped}/bin/clangd $@
''
