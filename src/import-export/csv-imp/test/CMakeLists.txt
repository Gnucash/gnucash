
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

GNC_ADD_TEST(test-csv-imp "${test_csv_imp_SOURCES}"
  CSV_IMP_TEST_INCLUDE_DIRS CSV_IMP_TEST_LIBS
  SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
)

