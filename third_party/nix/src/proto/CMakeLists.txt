# -*- mode: cmake; -*-
#
# The proto generation happens outside of CMake and the path to the
# generated files is passed in via the environment variable
# $NIX_PROTO_SRCS.
#
# This configuration defines a library target that compiles these
# sources and makes the headers available.

add_library(nixproto SHARED)
set_property(TARGET nixproto PROPERTY CXX_STANDARD 17)

set(HEADER_FILES
  $ENV{NIX_PROTO_SRCS}/libproto/common.pb.h
  $ENV{NIX_PROTO_SRCS}/libproto/worker.grpc.pb.h
  $ENV{NIX_PROTO_SRCS}/libproto/worker.pb.h
)

target_sources(nixproto
  PUBLIC
    ${HEADER_FILES}

  PRIVATE
    $ENV{NIX_PROTO_SRCS}/libproto/common.pb.cc
    $ENV{NIX_PROTO_SRCS}/libproto/worker.grpc.pb.cc
    $ENV{NIX_PROTO_SRCS}/libproto/worker.pb.cc
)

target_link_libraries(nixproto
  gRPC::grpc++_reflection
  protobuf::libprotobuf
)

target_include_directories(nixproto
  INTERFACE $ENV{NIX_PROTO_SRCS}
)

INSTALL(FILES ${HEADER_FILES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/nix/libproto)
INSTALL(TARGETS nixproto DESTINATION ${CMAKE_INSTALL_LIBDIR})
