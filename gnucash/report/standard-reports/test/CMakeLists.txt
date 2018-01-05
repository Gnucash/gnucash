SET(scm_test_standard_reports_SOURCES
  test-cash-flow.scm
  test-cashflow-barchart.scm
  test-standard-category-report.scm
  test-standard-net-barchart.scm
  test-standard-net-linechart.scm
)

set(scm_test_with_srfi64_SOURCES
  test-transaction.scm
)

SET(scm_test_report_SUPPORT
  test-generic-category-report.scm
  test-generic-net-barchart.scm
  test-generic-net-linechart.scm
  )

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-engine
  scm-test-engine
  scm-gettext
  scm-scm
  scm-test-report-system
  scm-standard-reports
  scm-report-stylesheets
  )

if (HAVE_SRFI64)
  gnc_add_scheme_tests("${scm_test_with_srfi64_SOURCES}")
endif (HAVE_SRFI64)

GNC_ADD_SCHEME_TESTS("${scm_test_standard_reports_SOURCES}")

GNC_ADD_SCHEME_TARGETS(scm-test-standard-support
  "${scm_test_report_SUPPORT}"
  "gnucash/report/standard-reports/test"
  "${GUILE_DEPENDS}"
  FALSE
  )
GNC_ADD_SCHEME_TARGETS(scm-test-standard-reports
  "${scm_test_standard_reports_SOURCES}"
  gnucash/report/standard-reports/test
  "scm-test-standard-support"
  FALSE
)

add_dependencies(check scm-test-standard-reports)

SET_DIST_LIST(test_standard_reports_DIST CMakeLists.txt 
  ${scm_test_with_srfi64_SOURCES}
  ${scm_test_standard_reports_SOURCES} ${scm_test_report_SUPPORT})
