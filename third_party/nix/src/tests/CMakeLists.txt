# -*- mode: cmake; -*-
include_directories(${PROJECT_BINARY_DIR}) # for 'generated/'

add_executable(attr-set attr-set.cc)
target_link_libraries(attr-set
  nixexpr
  rapidcheck
  GTest::gtest_main
)

gtest_discover_tests(attr-set)

add_executable(value-to-json value-to-json.cc)
target_link_libraries(value-to-json
  nixexpr
  nixstore
  GTest::gtest_main
)

gtest_discover_tests(value-to-json)

add_executable(language-tests language-tests.cc)
target_link_libraries(language-tests
  nixexpr
  nixstore
  GTest::gtest_main
)

gtest_discover_tests(language-tests)
