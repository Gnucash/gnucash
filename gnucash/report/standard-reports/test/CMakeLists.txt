GNC_ADD_SCHEME_TEST(test-cash-flow test-cash-flow.scm)
GNC_ADD_SCHEME_TEST(test-standard-category-report test-standard-category-report.scm)
GNC_ADD_SCHEME_TEST(test-standard-net-barchart test-standard-net-barchart.scm)
GNC_ADD_SCHEME_TEST(test-standard-net-linechart test-standard-net-linechart.scm)

SET(scm_test_standard_reports_SOURCES
  test-generic-category-report.scm
  test-generic-net-barchart.scm
  test-generic-net-linechart.scm
)

GNC_ADD_SCHEME_TARGETS(scm-test-standard-reports
  "${scm_test_standard_reports_SOURCES}"
  gnucash/report/standard-reports/test
  ""
  ""
  ""
  "scm-gnc-module;scm-test-report-system"
  FALSE
)

SET_DIST_LIST(test_standard_reports_DIST CMakeLists.txt Makefile.am ${scm_test_standard_reports_SOURCES}
        test-cash-flow.scm test-standard-category-report.scm test-standard-net-barchart.scm
        test-standard-net-linechart.scm)
