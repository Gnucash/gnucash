set(test_aqb_SOURCES test-aqb.c test-kvp.c)

set(test_aqb_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/gnucash/gnome
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-core
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${AQBANKING_INCLUDE_DIRS}
)

set(test_aqb_LIBS
  gncmod-aqbanking gnc-generic-import gnc-gnome gnc-gnome-utils
  gnc-ledger-core gnc-app-utils
  gnc-backend-xml-utils gnc-engine gnc-core-utils gnc-module
)

set_dist_list(test_aqb_DIST ${test_aqb_SOURCES} file-book.gnucash
  file-book-hbcislot.gnucash CMakeLists.txt )
if(WITH_AQBANKING)
  gnc_add_test(test-aqb
    "${test_aqb_SOURCES}"
    test_aqb_INCLUDE_DIRS
    test_aqb_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR})
 endif()
