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
  ${GNOME_UTILS_GUI_TEST_LIBS}
  ${GLIB2_INCLUDE_DIRS}
)

set (test_assistant-stock-transaction_LIBS
  gnc-engine
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
  ${test_gnome_scheme_SOURCES}
  ${test_gnome_SOURCES}
  ${test_assistant-stock-transaction_SOURCES}
)
