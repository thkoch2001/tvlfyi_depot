# -*- mode: cmake; -*-
cmake_minimum_required(VERSION 3.16)
project(nix CXX)
set(CMAKE_CXX_STANDARD 17)

# Export compile_commands.json which can be used by tools such as
# clangd and clang-tidy.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Provide an output path for pkgconfig.
include(GNUInstallDirs)
set(PKGCONFIG_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

# The following lines import CMake-native dependencies which may
# contain useful definitions. Other dependencies are not treated
# specially by CMake and are only linked into the resulting binary.
find_package(BZip2)
find_package(Boost COMPONENTS context)
find_package(CURL)
find_package(LibLZMA)
find_package(Protobuf REQUIRED)
find_package(SQLite3)
find_package(Threads)
find_package(absl REQUIRED)
find_package(gRPC REQUIRED)
find_package(glog REQUIRED)

# generate a configuration file (autoheader-style) to configure
# certain symbols that Nix depends on.
configure_file(config.h.in nix_config.h @ONLY)
INSTALL(FILES "${PROJECT_BINARY_DIR}/nix_config.h" DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/nix")

# install corepkgs
configure_file(corepkgs/config.nix.in config.nix @ONLY)
INSTALL(DIRECTORY corepkgs
  DESTINATION ${CMAKE_INSTALL_DATADIR}/nix
  FILES_MATCHING
    PATTERN "*.nix")
INSTALL(FILES "${PROJECT_BINARY_DIR}/config.nix" DESTINATION "${CMAKE_INSTALL_DATADIR}/nix/corepkgs")

# Conditionally run tests
option(PACKAGE_TESTS "Build the tests" ON)
if (PACKAGE_TESTS)
  enable_testing()
  find_package(GTest)
  find_package(rapidcheck)
  include(GoogleTest)
endif()

add_subdirectory(src)
