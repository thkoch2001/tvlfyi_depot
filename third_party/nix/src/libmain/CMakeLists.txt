# -*- mode: cmake; -*-
add_library(nixmain SHARED)
set_property(TARGET nixmain PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR}) # for config.h
target_include_directories(nixmain PUBLIC "${nix_SOURCE_DIR}/src")

set(HEADER_FILES
    common-args.hh
    shared.hh
)

target_sources(nixmain
  PUBLIC
    ${HEADER_FILES}
  PRIVATE
    common-args.cc
    shared.cc
    stack.cc
)

target_link_libraries(nixmain
  nixstore
  nixutil

  absl::strings
  glog
)

INSTALL(FILES ${HEADER_FILES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/nix/libmain)
INSTALL(TARGETS nixmain DESTINATION lib)
