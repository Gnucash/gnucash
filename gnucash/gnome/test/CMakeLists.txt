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
        ${GUILE_LDFLAGS})

macro(add_gnome_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
endmacro()

add_gnome_test(test-new-user-response test-new-user-response.cpp)
set_tests_properties(test-new-user-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-sx-creation-error-response test-sx-creation-error-response.cpp)
set_tests_properties(test-sx-creation-error-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-tax-table-rename-response test-tax-table-rename-response.cpp)
set_tests_properties(test-tax-table-rename-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(test-tax-table-entry-response test-tax-table-entry-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(test-tax-table-entry-response scm-expressions)
set_tests_properties(test-tax-table-entry-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-billterms-response test-billterms-response.cpp)
set_tests_properties(test-billterms-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(test-payment-split-response test-payment-split-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(test-payment-split-response scm-expressions)
set_tests_properties(test-payment-split-response PROPERTIES TIMEOUT 30)

set(INVOICE_ENTRY_DELETE_TEST_SOURCES
  test-invoice-entry-delete-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
set(INVOICE_ENTRY_DELETE_TEST_INCLUDE_DIRS
  ${GNOME_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
)
gnc_add_test_with_guile(test-invoice-entry-delete-response
  "${INVOICE_ENTRY_DELETE_TEST_SOURCES}"
  INVOICE_ENTRY_DELETE_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(test-invoice-entry-delete-response scm-expressions)
set_tests_properties(test-invoice-entry-delete-response PROPERTIES TIMEOUT 30)

set(INVOICE_POST_TEST_SOURCES
  test-invoice-post-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
set(INVOICE_POST_TEST_INCLUDE_DIRS
  ${GNOME_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
)
gnc_add_test_with_guile(test-invoice-post-response
  "${INVOICE_POST_TEST_SOURCES}"
  INVOICE_POST_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(test-invoice-post-response scm-expressions)
set_tests_properties(test-invoice-post-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-print-check-title-response test-print-check-title-response.cpp)
set_tests_properties(test-print-check-title-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-date-close-response test-date-close-response.cpp)
set_tests_properties(test-date-close-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-commodities-namespace-response test-commodities-namespace-response.cpp)
set_tests_properties(test-commodities-namespace-response PROPERTIES TIMEOUT 30)

set(BUDGET_SELECTION_TEST_SOURCES
  test-budget-selection-response.cpp
  ${CMAKE_BINARY_DIR}/gnucash/gnucash-gresources.c
)
gnc_add_test_with_guile(test-budget-selection-response
  "${BUDGET_SELECTION_TEST_SOURCES}" GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
add_dependencies(test-budget-selection-response scm-expressions)
set_tests_properties(test-budget-selection-response PROPERTIES TIMEOUT 30)

add_gnome_test(test-price-remove-response test-price-remove-response.cpp)
set_tests_properties(test-price-remove-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile(test-style-sheet-new-response test-style-sheet-new-response.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
set_tests_properties(test-style-sheet-new-response PROPERTIES TIMEOUT 30)
add_dependencies(test-style-sheet-new-response scm-report scm-report-stylesheets scm-reports)

foreach(preference_test IN ITEMS test-tax-table-rename-response
    test-tax-table-entry-response test-billterms-response
    test-payment-split-response test-invoice-entry-delete-response
    test-invoice-post-response
    test-print-check-title-response test-commodities-namespace-response
    test-style-sheet-new-response test-price-remove-response
    test-budget-selection-response)
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
  test-new-user-response.cpp
  test-sx-creation-error-response.cpp
  test-tax-table-rename-response.cpp
  test-tax-table-entry-response.cpp
  test-billterms-response.cpp
  test-invoice-entry-delete-response.cpp
  test-invoice-post-response.cpp
  test-payment-split-response.cpp
  test-print-check-title-response.cpp
  test-date-close-response.cpp
  test-commodities-namespace-response.cpp
  test-budget-selection-response.cpp
  test-price-remove-response.cpp
  test-style-sheet-new-response.cpp
  ${test_gnome_scheme_SOURCES}
  ${test_gnome_SOURCES}
  ${test_assistant-stock-transaction_SOURCES}
)
