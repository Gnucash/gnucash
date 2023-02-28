
set(CSV_IMP_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
)
set(CSV_IMP_TEST_LIBS gnc-csv-import gnc-engine test-core)

# This test does not run in Win32
if (NOT WIN32)
  set(MODULEPATH ${CMAKE_SOURCE_DIR}/gnucash/import-export/csv-imp)
  set(gtest_csv_imp_LIBS gnc-csv-import PkgConfig::GLIB2 gtest)
  set(gtest_csv_imp_INCLUDES
    ${MODULEPATH}
    ${CSV_IMP_TEST_INCLUDE_DIRS})

  set(test_tokenizer_SOURCES
    test-tokenizer.cpp)
  gnc_add_test(test-tokenizer "${test_tokenizer_SOURCES}"
    gtest_csv_imp_INCLUDES gtest_csv_imp_LIBS
    SRCDIR=${CMAKE_SOURCE_DIR}/gnucash/import-export/csv-imp/test)

  # Disable for now - there are no tests added yet to this source file
  #set(test_tx_import_SOURCES
  #  test-tx-import.cpp)
  #gnc_add_test(test-tx_import "${test_tx_import_SOURCES}"
  #  gtest_csv_imp_INCLUDES gtest_csv_imp_LIBS)
endif()

set(GNC_IMP_PROPS_TX_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/csv-imp
  ${GTEST_INCLUDE_DIR}
)

set(GNC_IMP_PROPS_TX_TEST_LIBS
  gnc-engine
  test-core
  gnc-generic-import
  gtest)

set(GNC_IMP_PROPS_TX_TEST_SOURCES
  ../gnc-imp-props-tx.cpp
  gtest-gnc-imp-props-tx.cpp
)

gnc_add_test(test-gnc-imp-props-tx "${GNC_IMP_PROPS_TX_TEST_SOURCES}"
  GNC_IMP_PROPS_TX_TEST_INCLUDE_DIRS GNC_IMP_PROPS_TX_TEST_LIBS)

set_dist_list(test_csv_import_DIST CMakeLists.txt
    test-tx-import.cpp test-tokenizer.cpp gtest-gnc-imp-props-tx.cpp
    sample1.csv ${test_csv_imp_SOURCES})
