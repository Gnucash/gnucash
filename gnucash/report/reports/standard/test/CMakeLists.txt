set(scm_test_reports_standard_SOURCES
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
  test-ifrs-cost-basis.scm
  test-invoice.scm
  test-new-owner-report.scm
  test-owner-report.scm
  test-portfolios.scm
)

set(scm_test_with_textual_ports_SOURCES
    test-stress-options.scm
)

set(GUILE_DEPENDS
  scm-app-utils
  scm-engine
  scm-test-engine
  scm-core-utils
  scm-test-report
  scm-reports
  scm-reports-standard-with-exposed-generator
  scm-report-stylesheets
  )

if (HAVE_SRFI64)
    gnc_add_scheme_test_targets(scm-test-reports-standard-srfi64
        SOURCES "${scm_test_with_srfi64_SOURCES}"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")
    gnc_add_scheme_tests("${scm_test_with_srfi64_SOURCES}")

    if (HAVE_TEXT_PORTS)
        gnc_add_scheme_test_targets(scm-test-reports-standard-tp
            SOURCES "${scm_test_with_textual_ports_SOURCES}"
            OUTPUT_DIR "tests"
            DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")
        gnc_add_scheme_tests("${scm_test_with_textual_ports_SOURCES}")
    endif()
endif()

gnc_add_scheme_tests("${scm_test_reports_standard_SOURCES}")

gnc_add_scheme_test_targets(scm-test-reports-standard
    SOURCES "${scm_test_reports_standard_SOURCES}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

add_dependencies(check scm-test-reports-standard)

set_dist_list(test_reports_standard_DIST CMakeLists.txt
  ${scm_test_with_srfi64_SOURCES}
  ${scm_test_with_textual_ports_SOURCES}
  ${scm_test_reports_standard_SOURCES})
