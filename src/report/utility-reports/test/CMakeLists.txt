CONFIGURE_FILE(test-load-module.in test-load-module @ONLY)
GNC_ADD_SCHEME_TEST(test-load-module-report-utility-reports
  test-load-module.in
)

SET_DIST_LIST(test_utility_reports_DIST CMakeLists.txt Makefile.am test-load-module.in)