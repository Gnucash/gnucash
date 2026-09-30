set(test_aqb_SOURCES test-aqb.c test-kvp.c test-standing-orders.c)

set(test_aqb_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb
  ${CMAKE_SOURCE_DIR}/common/test-core
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
  ${GWENHYWFAR_INCLUDE_DIRS}
  ${GTEST_INCLUDE_DIR}
)

set(test_aqb_LIBS
  gncmod-aqbanking gnc-generic-import gnc-gnome gnc-gnome-utils
  gnc-ledger-core gnc-app-utils
  gnc-backend-xml-utils gnc-engine gnc-core-utils gnc-module
)
set(test_aqb_RESPONSE_LIBS ${test_aqb_LIBS} gtest)

set_dist_list(test_aqb_DIST ${test_aqb_SOURCES} test-gwen-worker-response.cpp
  test-ab-import-context-response.cpp test-gwen-async-dialog.cpp
  test-gwen-async-dialog.dlg file-book.gnucash
  file-book-hbcislot.gnucash CMakeLists.txt )
if(WITH_AQBANKING)
  gnc_add_test(test-aqb
    "${test_aqb_SOURCES}"
    test_aqb_INCLUDE_DIRS
    test_aqb_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR})
  gnc_add_test(test-gwen-worker-response
    "${CMAKE_CURRENT_SOURCE_DIR}/test-gwen-worker-response.cpp"
    test_aqb_INCLUDE_DIRS
    test_aqb_RESPONSE_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  gnc_add_test(test-ab-import-context-response
    "${CMAKE_CURRENT_SOURCE_DIR}/test-ab-import-context-response.cpp"
    test_aqb_INCLUDE_DIRS
    test_aqb_RESPONSE_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  gnc_add_test(test-gwen-async-dialog
    "${CMAKE_CURRENT_SOURCE_DIR}/test-gwen-async-dialog.cpp"
    test_aqb_INCLUDE_DIRS
    test_aqb_RESPONSE_LIBS SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  if(COMPILE_GSCHEMAS)
    foreach(test_target test-gwen-worker-response test-ab-import-context-response
                        test-gwen-async-dialog)
      add_dependencies(${test_target} compiled-schemas)
    endforeach()
  endif()
 endif()
