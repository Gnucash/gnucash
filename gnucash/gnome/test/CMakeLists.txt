set(MODULEPATH ${CMAKE_SOURCE_DIR}/gnucash/gnome/)

set(GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/gnucash/gnome/
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils/
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
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

add_gnome_test(test-dialog-date-close test-dialog-date-close.c)
add_gnome_test(test-dialog-lot-viewer-scrub test-dialog-lot-viewer-scrub.c)
add_gnome_test(test-budget-view-column-ownership test-budget-view-column-ownership.c)
set_property(TEST test-budget-view-column-ownership APPEND PROPERTY ENVIRONMENT
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)
add_gnome_test(test-plugin-page-budget-window-lifetime test-plugin-page-budget-window-lifetime.c)
set_property(TEST test-plugin-page-budget-window-lifetime APPEND PROPERTY ENVIRONMENT
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set(test_dialog_sx_since_last_run_ownership_INCLUDE_DIRS
  ${GNOME_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
)
gnc_add_test_with_guile(test-dialog-sx-since-last-run-ownership
  "test-dialog-sx-since-last-run-ownership.c;test-sx-variable-fixture.cpp"
  test_dialog_sx_since_last_run_ownership_INCLUDE_DIRS GNOME_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)
# The scheduled-transaction fixture loads gnucash/app-utils/fin at runtime.
add_dependencies(test-dialog-sx-since-last-run-ownership scm-expressions)

gnc_add_test_with_guile(test-assistant-hierarchy-ownership
  test-assistant-hierarchy-ownership.cpp GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
set_property(TEST test-assistant-hierarchy-ownership APPEND PROPERTY ENVIRONMENT
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

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
  test-dialog-date-close.c
  test-dialog-lot-viewer-scrub.c
  test-budget-view-column-ownership.c
  test-plugin-page-budget-window-lifetime.c
  test-dialog-sx-since-last-run-ownership.c
  test-sx-variable-fixture.h
  test-sx-variable-fixture.cpp
  test-assistant-hierarchy-ownership.cpp
  ${test_gnome_scheme_SOURCES}
  ${test_gnome_SOURCES}
  ${test_assistant-stock-transaction_SOURCES}
)
