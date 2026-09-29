
set(SPLIT_REG_TEST_SOURCES
    test-split-register.c
    utest-split-register-copy-ops.c
)

set(SPLIT_REG_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
    ${CMAKE_BINARY_DIR}/common # for config.h
    ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
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

set(READONLY_THRESHOLD_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
    ${CMAKE_BINARY_DIR}/common # for config.h
)

set(READONLY_THRESHOLD_TEST_LIBS
    gnc-engine
    gnc-ledger-core
)

gnc_add_test(test-readonly-threshold-response
    test-readonly-threshold-response.cpp
    READONLY_THRESHOLD_TEST_INCLUDE_DIRS
    READONLY_THRESHOLD_TEST_LIBS
)
set_tests_properties(test-readonly-threshold-response PROPERTIES TIMEOUT 30)

set(ENTRY_LEDGER_CLOSE_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
    ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
    ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
    ${CMAKE_BINARY_DIR}/common
    ${GUILE_INCLUDE_DIRS}
)
set(ENTRY_LEDGER_CLOSE_TEST_LIBS gnc-engine gnc-ledger-core)
gnc_add_test_with_guile(test-entry-ledger-close-response
    test-entry-ledger-close-response.cpp
    ENTRY_LEDGER_CLOSE_TEST_INCLUDE_DIRS ENTRY_LEDGER_CLOSE_TEST_LIBS)
add_dependencies(test-entry-ledger-close-response scm-expressions)
set_tests_properties(test-entry-ledger-close-response PROPERTIES TIMEOUT 30)
set_property(TEST test-entry-ledger-close-response APPEND PROPERTY ENVIRONMENT
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(test-entry-ledger-close-response compiled-schemas)
endif()

set(RECN_CELL_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
    ${CMAKE_BINARY_DIR}/common # for config.h
)
set(RECN_CELL_TEST_LIBS gnc-ledger-core)
gnc_add_test(test-recncell-deferred
    test-recncell-deferred.cpp
    RECN_CELL_TEST_INCLUDE_DIRS RECN_CELL_TEST_LIBS)

set(TABLE_DEFERRED_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
    ${CMAKE_BINARY_DIR}/common # for config.h
)
set(TABLE_DEFERRED_TEST_LIBS gnc-register-core)
gnc_add_test(test-table-deferred-confirm
    test-table-deferred-confirm.cpp
    TABLE_DEFERRED_TEST_INCLUDE_DIRS TABLE_DEFERRED_TEST_LIBS)
set_tests_properties(test-table-deferred-confirm PROPERTIES TIMEOUT 30)

set_dist_list(test_ledger_core_DIST CMakeLists.txt ${SPLIT_REG_TEST_SOURCES}
    test-readonly-threshold-response.cpp
    test-entry-ledger-close-response.cpp)
list(APPEND test_ledger_core_DIST test-recncell-deferred.cpp
    test-table-deferred-confirm.cpp)
