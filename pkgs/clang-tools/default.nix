{ pkgs }:
with pkgs;

runCommand "clang-tools" {} ''
  mkdir -p $out/bin
  export libc_includes="${stdenv.lib.getDev stdenv.cc.libc}/include"
  export libcpp_includes="${llvmPackages.libcxx}/include/c++/v1"

  export clang=${llvmPackages.clang-unwrapped}

  echo $clang

  substituteAll ${./wrapper} $out/bin/clangd
  chmod +x $out/bin/clangd
  for tool in \
    clang-apply-replacements \
    clang-check \
    clang-format \
    clang-rename \
    clang-tidy
  do
    ln -s $out/bin/clangd $out/bin/$tool
  done
''
