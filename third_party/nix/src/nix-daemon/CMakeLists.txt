# -*- mode: cmake; -*-

# The nix-daemon is the binary running the gRPC server component to
# which other components of Nix talk to perform store and builder
# operations.

add_executable(nix-daemon)
include_directories(${PROJECT_BINARY_DIR}) # for config.h
set_property(TARGET nix-daemon PROPERTY CXX_STANDARD 17)

pkg_check_modules(systemd REQUIRED)

target_sources(nix-daemon
  PRIVATE
    nix-daemon-proto.hh
    nix-daemon-proto.cc
    nix-daemon.cc
)

target_link_libraries(nix-daemon
  nixutil
  nixstore
  nixmain
  absl::flags
  absl::flags_parse
  systemd
)

install(TARGETS nix-daemon DESTINATION bin)
