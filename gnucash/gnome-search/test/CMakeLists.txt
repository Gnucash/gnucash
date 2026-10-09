set(GNOME_SEARCH_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-search
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${GUILE_INCLUDE_DIRS}
)
set(GNOME_SEARCH_TEST_LIBS test-core gnc-gnome-search gtest)

gnc_add_test(gtest-search-account-response
  gtest-search-account-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(gtest-search-account-response PROPERTIES TIMEOUT 30)
set_property(TEST gtest-search-account-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(gtest-search-account-response compiled-schemas)
endif()

gnc_add_test(gtest-search-string-response
  gtest-search-string-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(gtest-search-string-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(gtest-search-number-response
  gtest-search-number-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(gtest-search-number-response PROPERTIES TIMEOUT 30)
add_dependencies(gtest-search-number-response scm-expressions)

set_dist_list(test_gnome_search_DIST
  CMakeLists.txt
  gtest-search-account-response.cpp
  gtest-search-string-response.cpp
  gtest-search-number-response.cpp)
