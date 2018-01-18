
SET(LEDGER_CORE_TEST_INCLUDE_DIRS "")
SET(LEDGER_CORE_TEST_LIBS)

GNC_ADD_TEST(test-link-module-ledger-core test-link-module.c
  LEDGER_CORE_TEST_INCLUDE_DIRS LEDGER_CORE_TEST_LIBS
)

SET_DIST_LIST(test_ledger_core_DIST CMakeLists.txt test-link-module.c)