SET(REPORT_GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${GUILE_INCLUDE_DIRS}

)
SET(REPORT_GNOME_TEST_LIBS gnc-module)

GNC_ADD_TEST_WITH_GUILE(test-link-module-report-gnome test-link-module.c
  REPORT_GNOME_TEST_INCLUDE_DIRS REPORT_GNOME_TEST_LIBS
)

GNC_ADD_SCHEME_TEST(test-load-module-report-gnome
  test-load-module.in
)

CONFIGURE_FILE(test-load-module.in test-load-module @ONLY)

SET_DIST_LIST(test_report_gnome_DIST CMakeLists.txt Makefile.am test-link-module.c test-load-module.in)

