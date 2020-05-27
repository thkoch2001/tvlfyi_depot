# -*- mode: cmake; -*-
add_library(nixutil SHARED)
set_property(TARGET nixutil PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR}) # for config.h
target_compile_features(nixutil PUBLIC cxx_std_17)

target_sources(nixutil
  PUBLIC
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
    prefork-compat.hh
    ref.hh
    serialise.hh
    sync.hh
    thread-pool.hh
    types.hh
    util.hh
    xml-writer.hh

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
  absl::strings
  glog
)

# Install header files to include/libutil and mark them for automatic
# inclusion in targets that link to this one.
target_include_directories(nixutil PUBLIC "${nix_SOURCE_DIR}/src")
INSTALL(TARGETS nixutil DESTINATION lib)
