# -*- mode: cmake; -*-
add_library(nixexpr SHARED)
set_property(TARGET nixexpr PROPERTY CXX_STANDARD 17)
include_directories(${PROJECT_BINARY_DIR}) # for 'generated/'
target_include_directories(nixexpr PUBLIC "${nix_SOURCE_DIR}/src")

# Generate lexer & parser for inclusion:
find_package(BISON)
find_package(FLEX)

BISON_TARGET(NixParser parser.y
  ${PROJECT_BINARY_DIR}/generated/parser-tab.cc
  DEFINES_FILE ${PROJECT_BINARY_DIR}/generated/parser-tab.hh)

FLEX_TARGET(NixLexer lexer.l
  ${PROJECT_BINARY_DIR}/generated/lexer-tab.cc
  DEFINES_FILE ${PROJECT_BINARY_DIR}/generated/lexer-tab.hh)

ADD_FLEX_BISON_DEPENDENCY(NixLexer NixParser)

set(HEADER_FILES
    attr-path.hh
    attr-set.hh
    common-eval-args.hh
    eval.hh
    eval-inline.hh
    function-trace.hh
    get-drvs.hh
    json-to-value.hh
    names.hh
    nixexpr.hh
    parser.hh
    primops.hh
    symbol-table.hh
    value.hh
    value-to-json.hh
    value-to-xml.hh
)

target_sources(nixexpr
  PUBLIC
    ${HEADER_FILES}

  PRIVATE
    ${PROJECT_BINARY_DIR}/generated/parser-tab.hh
    ${PROJECT_BINARY_DIR}/generated/parser-tab.cc
    ${PROJECT_BINARY_DIR}/generated/lexer-tab.hh
    ${PROJECT_BINARY_DIR}/generated/lexer-tab.cc
    primops/context.cc
    primops/fetchGit.cc
    primops/fetchMercurial.cc
    primops/fromTOML.cc
    attr-path.cc
    attr-set.cc
    common-eval-args.cc
    eval.cc
    function-trace.cc
    get-drvs.cc
    json-to-value.cc
    names.cc
    nixexpr.cc
    parser.cc
    primops.cc
    symbol-table.cc
    value.cc
    value-to-json.cc
    value-to-xml.cc
)

target_link_libraries(nixexpr
  nixmain
  nixstore
  nixutil

  absl::btree
  absl::flat_hash_set
  absl::node_hash_set
  absl::strings
)

configure_file("nix-expr.pc.in" "${PROJECT_BINARY_DIR}/nix-expr.pc" @ONLY)
INSTALL(FILES "${PROJECT_BINARY_DIR}/nix-expr.pc" DESTINATION "${PKGCONFIG_INSTALL_DIR}")

INSTALL(FILES ${HEADER_FILES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/nix/libexpr)
INSTALL(TARGETS nixexpr DESTINATION ${CMAKE_INSTALL_LIBDIR})
