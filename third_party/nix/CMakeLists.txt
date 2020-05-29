# -*- mode: cmake; -*-
cmake_minimum_required(VERSION 3.16)
project(nix CXX)
set(CMAKE_CXX_STANDARD 17)

# The following lines import CMake-native dependencies which may
# contain useful definitions. Other dependencies are not treated
# specially by CMake and are only linked into the resulting binary.
find_package(BZip2)
find_package(Boost COMPONENTS context) # probably coroutine + ::headers, lets find out
find_package(CURL)
find_package(SQLite3)
find_package(Threads)
find_package(LibLZMA)

# Abseil really doesn't like being precompiled. It is included here as
# an imported CMake project (i.e. it will be built /with/ this
# project).
#
# In development mode, the 'abseil_cpp' folder is symlinked to
# ../abseil_cpp (the relative location in the depot). In derivation
# builds this symlink is automatically replaced with an appropriate
# link to the Abseil sources in the Nix store.
add_subdirectory(abseil_cpp)

# Linking precompiled glog binaries (linked against libstdc++) leads
# to working binaries that do not output log messages. It seems that
# in order to use glog with LLVM & libc++, one needs to make glog a
# part of the local project build.
add_subdirectory(glog)

# generate a configuration file (autoheader-style) to configure
# certain symbols that Nix depends on.
configure_file(config.h.in nix_config.h @ONLY)

add_subdirectory(src)
