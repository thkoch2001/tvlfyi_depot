# -*- mode: cmake; -*-
cmake_minimum_required(VERSION 3.16)
project(nix CXX)
set(CMAKE_CXX_STANDARD 17)

# Export compile_commands.json which can be used by tools such as
# clangd and clang-tidy.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Enable warnings
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")

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

find_program(CLANG_TIDY_PATH clang-tidy)
if (CLANG_TIDY_PATH)
  # TODO(kanepyork): figure out how to reenable
  #message("Found clang-tidy: ${CLANG_TIDY_PATH}")
  #set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_PATH};--line-filter=[{\"name\":\"src/cpptoml/cpptoml.h\"},{\"name\":\"google/protobuf/metadata_lite.h\"}]")

  # nix's toolchain has a problem with system header includes, so clang-tidy requires a manual -isystem
  if (DEFINED ENV{LIBCXX_INCLUDE})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -isystem $ENV{LIBCXX_INCLUDE}")
  endif()
endif()

if (DEFINED ENV{SANDBOX_SHELL})
  set(SANDBOX_SHELL ENV{SANDBOX_SHELL})
else()
  find_program(BUSYBOX busybox)
  if (BUSYBOX)
    set(SANDBOX_SHELL "${BUSYBOX}")
  else()
    message(FATAL_ERROR "Could not find busybox and SANDBOX_SHELL is not set")
  endif()
endif()

if (DISABLE_GC)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DDISABLE_GC -fsanitize=address -fstandalone-debug")
endif()

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
