
set(LEDGER_CORE_TEST_INCLUDE_DIRS "")
set(LEDGER_CORE_TEST_LIBS)

gnc_add_test(test-link-module-ledger-core test-link-module.c
  LEDGER_CORE_TEST_INCLUDE_DIRS LEDGER_CORE_TEST_LIBS
)

set_dist_list(test_ledger_core_DIST CMakeLists.txt test-link-module.c)