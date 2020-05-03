set(scm_test_standard_reports_SOURCES
  test-cash-flow.scm
)

set(scm_test_with_srfi64_SOURCES
  test-standard-category-report.scm
  test-standard-net-linechart.scm
  test-standard-net-barchart.scm
  test-cashflow-barchart.scm
  test-charts.scm
  test-transaction.scm
  test-account-summary.scm
  test-balsheet-pnl.scm
  test-income-gst.scm
  test-budget.scm
  test-register.scm
  test-trial-balance.scm
  test-equity-statement.scm
  test-average-balance.scm
  test-portfolios.scm
)

set(scm_test_with_textual_ports_SOURCES
    test-stress-options.scm
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
  if (HAVE_TEXT_PORTS)
    gnc_add_scheme_tests("${scm_test_with_textual_ports_SOURCES}")
  endif()
endif()

gnc_add_scheme_tests("${scm_test_standard_reports_SOURCES}")

gnc_add_scheme_targets(scm-test-standard-reports
  "${scm_test_standard_reports_SOURCES}"
  gnucash/report/standard-reports/test
  "${GUILE_DEPENDS}"
  FALSE
)

add_dependencies(check scm-test-standard-reports)

set_dist_list(test_standard_reports_DIST CMakeLists.txt
  ${scm_test_with_srfi64_SOURCES}
  ${scm_test_with_textual_ports_SOURCES}
  ${scm_test_standard_reports_SOURCES})
