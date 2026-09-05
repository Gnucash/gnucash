set(test_sie4_export_SOURCES
  test-sie4-export.cpp
)

set(test_sie4_export_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/sie4
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
)

set(test_sie4_export_LIBS
  gnc-sie4-export
  gnc-engine
  gtest
)

gnc_add_test(test-sie4-export
  "${test_sie4_export_SOURCES}"
  test_sie4_export_INCLUDE_DIRS
  test_sie4_export_LIBS
  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}
)
add_dependencies(test-sie4-export gncmod-backend-xml)

set_dist_list(test_sie4_export_DIST
  CMakeLists.txt
  ${test_sie4_export_SOURCES}
  test-books.gnucash
  fail-books.gnucash
  expected-test-books.se
)
