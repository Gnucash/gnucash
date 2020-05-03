
set(scm_test_business_reports_with_srfi64_SOURCES
  test-invoice.scm
  test-new-owner-report.scm
  test-owner-report.scm
)

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-engine
  scm-test-engine
  scm-gettext
  scm-scm
  scm-test-report-system
  scm-report-stylesheets
  )

if (HAVE_SRFI64)
  gnc_add_scheme_tests("${scm_test_business_reports_with_srfi64_SOURCES}")
endif()

gnc_add_scheme_targets(scm-test-business-reports
  "${scm_test_business_reports_SOURCES}"
  gnucash/report/business-reports/test
  "scm-test-business-support"
  FALSE
)

add_dependencies(check scm-test-business-reports)

set_dist_list(test_business_reports_DIST CMakeLists.txt
  ${scm_test_business_reports_with_srfi64_SOURCES}
  )
