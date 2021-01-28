
set(scm_test_report_SOURCES
  test-load-report-module.scm
  test-test-extras.scm
)

set (scm_test_report_with_srfi64_SOURCES
  test-commodity-utils.scm
  test-report-utilities.scm
  test-html-utilities-srfi64.scm
  test-html-fonts.scm
  test-html-chart.scm
  test-report-html.scm
  test-report.scm
  )

set(GUILE_DEPENDS
  scm-app-utils
  scm-core-utils
  scm-core-utils
  scm-engine
  scm-test-engine
  scm-report-2
  scm-test-report
  scm-report-stylesheets
  )
gnc_add_scheme_tests("${scm_test_report_SOURCES}")

if (HAVE_SRFI64)
    gnc_add_scheme_test_targets(scm-test-report-srfi64
        SOURCES "${scm_test_report_with_srfi64_SOURCES}"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")

    gnc_add_scheme_tests ("${scm_test_report_with_srfi64_SOURCES}")
endif()

gnc_add_scheme_test_targets(scm-test-report
    SOURCES "test-report-extras.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_test_targets(scm-test-report-2
    SOURCES "${scm_test_report_SOURCES}"
    OUTPUT_DIR "tests"
    DEPENDS "scm-test-report")

add_dependencies(check scm-test-report)

set_dist_list(test_report_DIST
  CMakeLists.txt
  ${scm_test_report_with_srfi64_SOURCES}
  ${scm_test_report_SOURCES}
  test-report-extras.scm
)
