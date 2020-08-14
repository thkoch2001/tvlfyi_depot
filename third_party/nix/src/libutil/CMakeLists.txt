# -*- mode: cmake; -*-
add_library(nixutil SHARED)
set_property(TARGET nixutil PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR}) # for config.h
target_compile_features(nixutil PUBLIC cxx_std_17)

set(HEADER_FILES
    affinity.hh
    archive.hh
    args.hh
    compression.hh
    config.hh
    finally.hh
    hash.hh
    istringstream_nocopy.hh
    json.hh
    lazy.hh
    lru-cache.hh
    monitor-fd.hh
    pool.hh
    proto.hh
    ref.hh
    serialise.hh
    status.hh
    sync.hh
    thread-pool.hh
    types.hh
    util.hh
    visitor.hh
    xml-writer.hh
)

target_sources(nixutil
  PUBLIC
    ${HEADER_FILES}

  PRIVATE
    affinity.cc
    archive.cc
    args.cc
    compression.cc
    config.cc
    hash.cc
    json.cc
    serialise.cc
    thread-pool.cc
    util.cc
    xml-writer.cc
)

target_link_libraries(nixutil
  nixproto
  absl::strings
  absl::statusor
  glog
  BZip2::BZip2
  LibLZMA::LibLZMA
  Boost::context
  brotlienc
  brotlidec
  ssl
)

# Install header files to include/libutil and mark them for automatic
# inclusion in targets that link to this one.
target_include_directories(nixutil PUBLIC "${nix_SOURCE_DIR}/src")
INSTALL(FILES ${HEADER_FILES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/nix/libutil)
INSTALL(TARGETS nixutil DESTINATION ${CMAKE_INSTALL_LIBDIR})
