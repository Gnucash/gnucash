
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

# This test does not run in Win32
IF (NOT WIN32)
  SET(MODULEPATH ${CMAKE_SOURCE_DIR}/src/import-export/csv-imp)
  SET(gtest_csv_imp_LIBS gncmod-csv-import ${GLIB2_LDFLAGS} ${GTEST_LIB})
  SET(gtest_csv_imp_INCLUDES
    ${MODULEPATH}
    ${CSV_IMP_TEST_INCLUDE_DIRS}
    ${GTEST_INCLUDE_DIR})

  SET(test_tokenizer_SOURCES
    test-tokenizer.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-tokenizer "${test_tokenizer_SOURCES}"
    gtest_csv_imp_INCLUDES gtest_csv_imp_LIBS
    SRCDIR=${CMAKE_SOURCE_DIR}/src/import-export/csv-imp/test)

  SET(test_tx_import_SOURCES
    test-tx-import.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-tx_import "${test_tx_import_SOURCES}"
    gtest_csv_imp_INCLUDES gtest_csv_imp_LIBS)
ENDIF()

SET_DIST_LIST(test_csv_import_DIST CMakeLists.txt Makefile.am sample1.csv ${test_csv_imp_SOURCES})