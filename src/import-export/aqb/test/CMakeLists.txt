set(test_aqb_SOURCES test-aqb.c test-kvp.c)

set(test_aqb_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/import-export/aqb
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/import-export
  ${CMAKE_SOURCE_DIR}/src/gnome
  ${CMAKE_SOURCE_DIR}/src/register/ledger-core
  ${CMAKE_SOURCE_DIR}/src/register/regiser-gnome
  ${CMAKE_SOURCE_DIR}/src/register/regitser-core
  ${CMAKE_SOURCE_DIR}/src/gnome-utils
  ${CMAKE_SOURCE_DIR}/src/app-utils
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/core-utils
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${GLIB2_INCLUDE_DIRS}
  ${GTK_INCLUDE_DIRS}
  ${AQBANKING_INCLUDE_DIRS}
)

set(test_aqb_LIBS
  gncmod-aqbanking gncmod-generic-import gnc-gnome gncmod-gnome-utils
  gncmod-ledger-core gncmod-report-gnome gncmod-app-utils
  gncmod-backend-xml-utils gncmod-engine gnc-core-utils gnc-module
  gnc-qof ${GLIB2_LDFLAGS}
)

SET_DIST_LIST(test_aqb_DIST ${test_aqb_SOURCES} file-book.gnucash
  file-book-hbcislot.gnucash CMakeLists.txt Makefile.am)
IF(WITH_AQBANKING)
  GNC_ADD_TEST(test-aqb
    "${test_aqb_SOURCES}"
    test_aqb_INCLUDE_DIRS
    test_aqb_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR})
 ENDIF()
