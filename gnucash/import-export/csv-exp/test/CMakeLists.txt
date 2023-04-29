
set (test-csv-export-helpers_SOURCES
  test-csv-export-helpers.cpp
)

set (test-csv-export-helpers_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
)

set (test-csv-export-helpers_LIBS
  gnc-csv-export
  gtest
)

gnc_add_test (test-csv-export-helpers
  "${test-csv-export-helpers_SOURCES}"
  test-csv-export-helpers_INCLUDE_DIRS
  test-csv-export-helpers_LIBS
)

set_dist_list (test_csv_export_DIST
  CMakeLists.txt
  ${test-csv-export-helpers_SOURCES}
)
