
set(LEDGER_CORE_TEST_INCLUDE_DIRS "")
set(LEDGER_CORE_TEST_LIBS)

gnc_add_test(test-link-module-ledger-core test-link-module.c
  LEDGER_CORE_TEST_INCLUDE_DIRS LEDGER_CORE_TEST_LIBS
)

set(SPLIT_REG_TEST_SOURCES
    test-split-register.c
    utest-split-register-copy-ops.c
)

set(SPLIT_REG_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
    ${CMAKE_BINARY_DIR}/common # for config.h
    ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
    ${GLIB2_INCLUDE_DIRS}
)

set(SPLIT_REG_TEST_LIBS
    gnc-engine
    gnc-ledger-core
)

gnc_add_test(test-split-register-copy-ops
    "${SPLIT_REG_TEST_SOURCES};${CMAKE_SOURCE_DIR}/common/test-core/unittest-support.c"
    SPLIT_REG_TEST_INCLUDE_DIRS
    SPLIT_REG_TEST_LIBS
)

set_dist_list(test_ledger_core_DIST CMakeLists.txt test-link-module.c ${SPLIT_REG_TEST_SOURCES})
