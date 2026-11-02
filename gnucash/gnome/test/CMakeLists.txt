set(MODULEPATH ${CMAKE_SOURCE_DIR}/gnucash/gnome/)

set(GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/gnucash/gnome/
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils/
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${GUILE_INCLUDE_DIRS}
)

set(GNOME_TEST_LIBS
        gnc-gnome
        gnc-test-engine
        test-core
        gtest
        ${GUILE_LDFLAGS})

macro(add_gnome_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
endmacro()

add_gnome_test(gtest-new-user-response gtest-new-user-response.cpp)
set_tests_properties(gtest-new-user-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-sx-creation-error-response gtest-sx-creation-error-response.cpp)
set_tests_properties(gtest-sx-creation-error-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-tax-table-rename-response gtest-tax-table-rename-response.cpp)
set_tests_properties(gtest-tax-table-rename-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(gtest-tax-table-entry-response gtest-tax-table-entry-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(gtest-tax-table-entry-response scm-expressions)
set_tests_properties(gtest-tax-table-entry-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-billterms-response gtest-billterms-response.cpp)
set_tests_properties(gtest-billterms-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(gtest-payment-split-response gtest-payment-split-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(gtest-payment-split-response scm-expressions)
set_tests_properties(gtest-payment-split-response PROPERTIES TIMEOUT 30)

set(INVOICE_ENTRY_DELETE_TEST_SOURCES
  gtest-invoice-entry-delete-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
set(INVOICE_ENTRY_DELETE_TEST_INCLUDE_DIRS
  ${GNOME_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
)
gnc_add_test_with_guile(gtest-invoice-entry-delete-response
  "${INVOICE_ENTRY_DELETE_TEST_SOURCES}"
  INVOICE_ENTRY_DELETE_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(gtest-invoice-entry-delete-response scm-expressions)
set_tests_properties(gtest-invoice-entry-delete-response PROPERTIES TIMEOUT 30)

set(INVOICE_POST_TEST_SOURCES
  gtest-invoice-post-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
set(INVOICE_POST_TEST_INCLUDE_DIRS
  ${GNOME_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
)
gnc_add_test_with_guile(gtest-invoice-post-response
  "${INVOICE_POST_TEST_SOURCES}"
  INVOICE_POST_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(gtest-invoice-post-response scm-expressions)
set_tests_properties(gtest-invoice-post-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-print-check-title-response gtest-print-check-title-response.cpp)
set_tests_properties(gtest-print-check-title-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-date-close-response gtest-date-close-response.cpp)
set_tests_properties(gtest-date-close-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-commodities-namespace-response gtest-commodities-namespace-response.cpp)
set_tests_properties(gtest-commodities-namespace-response PROPERTIES TIMEOUT 30)

set(BUDGET_SELECTION_TEST_SOURCES
  gtest-budget-selection-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
gnc_add_test_with_guile(gtest-budget-selection-response
  "${BUDGET_SELECTION_TEST_SOURCES}" GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(gtest-budget-selection-response scm-expressions)
set_tests_properties(gtest-budget-selection-response PROPERTIES TIMEOUT 30)

add_gnome_test(gtest-price-remove-response gtest-price-remove-response.cpp)
set_tests_properties(gtest-price-remove-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(gtest-style-sheet-new-response gtest-style-sheet-new-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
set_tests_properties(gtest-style-sheet-new-response PROPERTIES TIMEOUT 30)
add_dependencies(gtest-style-sheet-new-response scm-report scm-report-stylesheets scm-reports)

foreach(preference_test IN ITEMS gtest-tax-table-rename-response
    gtest-tax-table-entry-response gtest-billterms-response
    gtest-payment-split-response gtest-invoice-entry-delete-response
    gtest-invoice-post-response
    gtest-print-check-title-response gtest-commodities-namespace-response
    gtest-style-sheet-new-response gtest-price-remove-response
    gtest-budget-selection-response)
  set_property(TEST ${preference_test} APPEND PROPERTY ENVIRONMENT
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  if(COMPILE_GSCHEMAS)
    add_dependencies(${preference_test} compiled-schemas)
  endif()
endforeach()

gnc_add_test_with_guile(test-invoice-report-builtin-default test-invoice-report-builtin-default.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS
)

set(GUILE_DEPENDS
  scm-core-utils
  test-core
  gnc-test-engine
  gnc-app-utils
  gnc-core-utils
  gnc-engine
)

set (test_assistant-stock-transaction_SOURCES
  gtest-assistant-stock-transaction.cpp
)

set (test_assistant-stock-transaction_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${GNOME_UTILS_GUI_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
  ${GNOME_UTILS_GUI_TEST_LIBS}
  ${GLIB2_INCLUDE_DIRS}
)

set (test_assistant-stock-transaction_LIBS
  gnc-engine
  gnc-gnome
  gnc-gnome-utils
  gtest
)

gnc_add_test (test-assistant-stock-transaction
  "${test_assistant-stock-transaction_SOURCES}"
  test_assistant-stock-transaction_INCLUDE_DIRS
  test_assistant-stock-transaction_LIBS
)

set_dist_list(test_gnome_DIST
  CMakeLists.txt
  test-invoice-report-builtin-default.cpp
  gtest-new-user-response.cpp
  gnome-response-test-fixture.h
  gtest-sx-creation-error-response.cpp
  gtest-tax-table-rename-response.cpp
  gtest-tax-table-entry-response.cpp
  gtest-billterms-response.cpp
  gtest-invoice-entry-delete-response.cpp
  gtest-invoice-post-response.cpp
  gtest-payment-split-response.cpp
  gtest-print-check-title-response.cpp
  gtest-date-close-response.cpp
  gtest-commodities-namespace-response.cpp
  gtest-budget-selection-response.cpp
  gtest-price-remove-response.cpp
  gtest-style-sheet-new-response.cpp
  ${test_gnome_scheme_SOURCES}
  ${test_gnome_SOURCES}
  ${test_assistant-stock-transaction_SOURCES}
)
