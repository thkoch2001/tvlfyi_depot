# -*- mode: cmake; -*-
cmake_minimum_required(VERSION 3.16)
project(dt)
add_executable(dt dt.cc)
find_package(absl REQUIRED)

target_link_libraries(dt
  absl::flags
  absl::flags_parse
  absl::hash
  absl::time
  absl::strings
  farmhash
)

install(TARGETS dt DESTINATION bin)
