# -*- mode: cmake; -*-
add_library(nixstore SHARED)
set_property(TARGET nixstore PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR}) # for config.h
target_include_directories(nixstore PUBLIC "${nix_SOURCE_DIR}/src")

# The database schema is stored in schema.sql, but needs to be
# available during the build as static data.
#
# These commands create an includeable source-file out of it.
file(READ "schema.sql" NIX_SCHEMA)

string(CONFIGURE
  "#pragma once
   namespace nix {
   constexpr char kNixSqlSchema[] = R\"(${NIX_SCHEMA})\"\;
   }"
  NIX_SCHEMA_GEN)

file(WRITE ${PROJECT_BINARY_DIR}/generated/schema.sql.hh ${NIX_SCHEMA_GEN})

target_sources(nixstore
  PUBLIC
    binary-cache-store.hh
    builtins.hh
    crypto.hh
    derivations.hh
    download.hh
    fs-accessor.hh
    globals.hh
    local-store.hh
    machines.hh
    nar-accessor.hh
    nar-info-disk-cache.hh
    nar-info.hh
    parsed-derivations.hh
    pathlocks.hh
    profiles.hh
    references.hh
    remote-fs-accessor.hh
    remote-store.hh
    s3-binary-cache-store.hh
    s3.hh
    serve-protocol.hh
    sqlite.hh
    ssh.hh
    store-api.hh
    worker-protocol.hh

  PRIVATE
    ${PROJECT_BINARY_DIR}/generated/schema.sql.hh
    binary-cache-store.cc
    build.cc
    crypto.cc
    derivations.cc
    download.cc
    export-import.cc
    gc.cc
    globals.cc
    http-binary-cache-store.cc
    legacy-ssh-store.cc
    local-binary-cache-store.cc
    local-fs-store.cc
    local-store.cc
    machines.cc
    misc.cc
    nar-accessor.cc
    nar-info.cc
    nar-info-disk-cache.cc
    optimise-store.cc
    parsed-derivations.cc
    pathlocks.cc
    profiles.cc
    references.cc
    remote-fs-accessor.cc
    remote-store.cc
    s3-binary-cache-store.cc
    sqlite.cc
    ssh.cc
    ssh-store.cc
    store-api.cc
    builtins/buildenv.cc
    builtins/fetchurl.cc
)

target_link_libraries(nixstore
  nixutil

  BZip2::BZip2
  Boost::context
  CURL::libcurl
  LibLZMA::LibLZMA
  SQLite::SQLite3
  absl::strings
  brotlidec
  brotlienc
  glog
  seccomp
  sodium
)

INSTALL(TARGETS nixstore DESTINATION lib)
