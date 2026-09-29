set(GNOME_SEARCH_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-search
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${GUILE_INCLUDE_DIRS}
)
set(GNOME_SEARCH_TEST_LIBS test-core gnc-gnome-search)

gnc_add_test(test-search-account-response
  test-search-account-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(test-search-account-response PROPERTIES TIMEOUT 30)
set_property(TEST test-search-account-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(test-search-account-response compiled-schemas)
endif()

gnc_add_test(test-search-string-response
  test-search-string-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(test-search-string-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(test-search-number-response
  test-search-number-response.cpp
  GNOME_SEARCH_TEST_INCLUDE_DIRS GNOME_SEARCH_TEST_LIBS)
set_tests_properties(test-search-number-response PROPERTIES TIMEOUT 30)
add_dependencies(test-search-number-response scm-expressions)

set_dist_list(test_gnome_search_DIST
  CMakeLists.txt
  test-search-account-response.cpp
  test-search-string-response.cpp
  test-search-number-response.cpp)
