SET(REPORT_GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}

)
SET(REPORT_GNOME_TEST_LIBS gnc-module)

GNC_ADD_TEST_WITH_GUILE(test-link-module-report-gnome test-link-module.c
  REPORT_GNOME_TEST_INCLUDE_DIRS REPORT_GNOME_TEST_LIBS
)

SET(GUILE_DEPENDS
  scm-app-utils
  gncmod-gnome-utils
  gnc-module
  gncmod-engine
  gncmod-report-gnome
)

GNC_ADD_SCHEME_TARGETS(scm-test-load-report-gnome-module
  "test-load-report-gnome-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)
GNC_ADD_SCHEME_TESTS(test-load-report-gnome-module.scm)

SET_DIST_LIST(test_report_gnome_DIST CMakeLists.txt test-link-module.c test-load-report-gnome-module.scm)

