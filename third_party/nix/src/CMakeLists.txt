# -*- mode: cmake; -*-

# The 'nix' binary is composed of various sources below this
# directory. In the previous build system, they were all built from
# this location and this setup mimics that (with the exception of the
# various Nix libraries).

add_subdirectory(libutil)
add_subdirectory(libstore)
add_subdirectory(libmain)
add_subdirectory(libexpr)

add_executable(nix)
set_property(TARGET nix PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR})
target_include_directories(nix PUBLIC "${nix_SOURCE_DIR}/src")

target_sources(nix
  PRIVATE
    nix/command.hh
    nix/legacy.hh
    nix-env/user-env.hh
    nix-store/dotgraph.hh
    nix-store/graphml.hh

    nix/add-to-store.cc
    nix/build.cc
    nix/cat.cc
    nix/command.cc
    nix/copy.cc
    nix/doctor.cc
    nix/dump-path.cc
    nix/edit.cc
    nix/eval.cc
    nix/hash.cc
    nix/installables.cc
    nix/legacy.cc
    nix/log.cc
    nix/ls.cc
    nix/main.cc
    nix/optimise-store.cc
    nix/path-info.cc
    nix/ping-store.cc
    nix/repl.cc
    nix/run.cc
    nix/search.cc
    nix/show-config.cc
    nix/show-derivation.cc
    nix/sigs.cc
    nix/upgrade-nix.cc
    nix/verify.cc
    nix/why-depends.cc

    build-remote/build-remote.cc
    nix-build/nix-build.cc
    nix-channel/nix-channel.cc
    nix-collect-garbage/nix-collect-garbage.cc
    nix-copy-closure/nix-copy-closure.cc
    nix-daemon/nix-daemon.cc
    nix-env/nix-env.cc
    nix-env/user-env.cc
    nix-instantiate/nix-instantiate.cc
    nix-prefetch-url/nix-prefetch-url.cc
    nix-store/dotgraph.cc
    nix-store/graphml.cc
    nix-store/nix-store.cc
)

target_link_libraries(nix
  nixexpr
  nixmain
  nixstore
  nixutil

  absl::strings
  editline
  glog
)

INSTALL(TARGETS nix DESTINATION bin)
