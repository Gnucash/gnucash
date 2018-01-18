set(test_aqb_SOURCES test-aqb.c test-kvp.c)

set(test_aqb_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/gnucash/gnome
  ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
  ${CMAKE_SOURCE_DIR}/gnucash/register/regiser-gnome
  ${CMAKE_SOURCE_DIR}/gnucash/register/regitser-core
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GTK_INCLUDE_DIRS}
  ${AQBANKING_INCLUDE_DIRS}
)

set(test_aqb_LIBS
  gncmod-aqbanking gncmod-generic-import gnc-gnome gncmod-gnome-utils
  gncmod-ledger-core gncmod-report-gnome gncmod-app-utils
  gncmod-backend-xml-utils gncmod-engine gnc-core-utils gnc-module
  ${GLIB2_LDFLAGS}
)

SET_DIST_LIST(test_aqb_DIST ${test_aqb_SOURCES} file-book.gnucash
  file-book-hbcislot.gnucash CMakeLists.txt )
IF(WITH_AQBANKING)
  GNC_ADD_TEST(test-aqb
    "${test_aqb_SOURCES}"
    test_aqb_INCLUDE_DIRS
    test_aqb_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR})
 ENDIF()
