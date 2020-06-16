with import <nixpkgs> {};
runCommand "clang-tools" {} ''
  mkdir -p $out/bin
  for file in ${clang-tools}/bin/*; do
    if [ $(basename "$file") != "clangd" ]; then
      ln -s "$file" $out/bin
    fi
  done

  sed \
    -e "18iexport CPLUS_INCLUDE_PATH=${llvmPackages.libcxx}/include/c++/v1\\''${CPATH:+':'}\\''${CPATH}" \
    -e '/CPLUS_INCLUDE_PATH/d' \
      < ${clang-tools}/bin/clangd \
      > $out/bin/clangd
''
