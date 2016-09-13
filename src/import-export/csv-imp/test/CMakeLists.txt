
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

ENDIF()

