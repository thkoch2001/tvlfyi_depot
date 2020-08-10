# -*- mode: cmake; -*-
include_directories(${PROJECT_BINARY_DIR}) # for 'generated/'

add_executable(attr-set attr-set.cc)
target_link_libraries(attr-set
  nixexpr
  rapidcheck
  rapidcheck_gtest
  GTest::gtest_main
)

gtest_discover_tests(attr-set)

add_executable(derivations_test derivations_test.cc)
target_link_libraries(derivations_test
  nixexpr
  nixstore
  rapidcheck
  rapidcheck_gtest
  GTest::gtest_main
)

gtest_discover_tests(derivations_test)

add_executable(hash_test hash_test.cc)
target_link_libraries(hash_test
  nixutil
  GTest::gtest_main
)

gtest_discover_tests(hash_test)

add_executable(references_test references_test.cc)
target_link_libraries(references_test
  nixstore
  rapidcheck
  rapidcheck_gtest
  GTest::gtest_main
)

gtest_discover_tests(references_test)

add_executable(store_test store_tests.cc)
target_link_libraries(store_test
  nixstore
  nixstoremock
  GTest::gtest_main
)

gtest_discover_tests(store_test)

add_executable(util_test util_tests.cc)
target_link_libraries(util_test
  nixutil
  GTest::gtest_main
)

gtest_discover_tests(store_test)

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
