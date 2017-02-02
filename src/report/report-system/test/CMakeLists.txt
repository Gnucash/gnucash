
SET(REPORT_SYSTEM_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${GUILE_INCLUDE_DIRS}
)
SET(REPORT_SYSTEM_TEST_LIBS gnc-module)

GNC_ADD_TEST_WITH_GUILE(test-link-module-report-system test-link-module.c
  REPORT_SYSTEM_TEST_INCLUDE_DIRS REPORT_SYSTEM_TEST_LIBS
)

GNC_ADD_SCHEME_TEST(test-load-module-report-system test-load-module.in)
GNC_ADD_SCHEME_TEST(test-collectors test-collectors.scm)
GNC_ADD_SCHEME_TEST(test-list-extras test-list-extras.scm)
GNC_ADD_SCHEME_TEST(test-report-utilities test-report-utilities.scm)
# This test is not run in the autotools build.
#GNC_ADD_SCHEME_TEST(test-test-extras test-test-extras.scm)

GNC_ADD_SCHEME_TARGETS(scm-test-report-system
  test-extras.scm
  gnucash/report/report-system/test
  ""
  ""
  ""
  "scm-gnc-module;scm-test-engine;scm-report-system-3"
  FALSE
)

CONFIGURE_FILE(test-load-module.in test-load-module @ONLY)

SET_DIST_LIST(test_report_system_DIST CMakeLists.txt Makefile.am test-collectors.scm test-extras.scm test-link-module.c
        test-load-module.in test-report-utilities.scm test-list-extras.scm)