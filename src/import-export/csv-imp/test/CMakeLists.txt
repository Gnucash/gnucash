
SET(CSV_IMP_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_SOURCE_DIR}/lib
  ${GLIB2_INCLUDE_DIRS}
)
SET(CSV_IMP_TEST_LIBS gncmod-csv-import gncmod-engine gnc-qof test-core)

SET(test_csv_imp_SOURCES
  test-csv-imp.c
  utest-gnc-csv-model.c
)

# This test does not run in Win32
IF (NOT WIN32)
  GNC_ADD_TEST(test-csv-imp "${test_csv_imp_SOURCES}"
    CSV_IMP_TEST_INCLUDE_DIRS CSV_IMP_TEST_LIBS
    SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
  )
ENDIF()

SET_DIST_LIST(test_csv_import_DIST CMakeLists.txt Makefile.am sample1.csv ${test_csv_imp_SOURCES})