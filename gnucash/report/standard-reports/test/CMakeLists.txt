SET(scm_test_standard_reports_SOURCES
  test-cash-flow.scm
  test-cashflow-barchart.scm
  test-standard-category-report.scm
  test-standard-net-barchart.scm
  test-standard-net-linechart.scm
)

SET(scm_test_report_SUPPORT
  test-generic-category-report.scm
  test-generic-net-barchart.scm
  test-generic-net-linechart.scm
  )


GNC_ADD_SCHEME_TESTS("${scm_test_standard_reports_SOURCES}")

GNC_ADD_SCHEME_TARGETS(scm-test-standard-support
  "${scm_test_report_SUPPORT}"
  "gnucash/report/standard-reports/test"
  "scm-gnc-module;scm-test-report-system;scm-standard-reports;scm-report-stylesheets-2"
  FALSE
  )
GNC_ADD_SCHEME_TARGETS(scm-test-standard-reports
  "${scm_test_standard_reports_SOURCES}"
  gnucash/report/standard-reports/test
  scm-test-standard-support
  FALSE
)

SET_DIST_LIST(test_standard_reports_DIST CMakeLists.txt 
  ${scm_test_standard_reports_SOURCES} ${scm_test_report_SUPPORT})
