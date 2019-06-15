
set(REPORT_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}
)
set(REPORT_TEST_LIBS gnc-module)

gnc_add_test_with_guile(test-link-module-report test-link-module.c
  REPORT_TEST_INCLUDE_DIRS REPORT_TEST_LIBS
)

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
  scm-gnc-module
  scm-app-utils
  scm-core-utils
  scm-gettext
  scm-engine
  scm-test-engine
  scm-scm
  scm-report-2
  scm-test-report
  )
gnc_add_scheme_tests("${scm_test_report_SOURCES}")

if (HAVE_SRFI64)
  gnc_add_scheme_test_targets(scm-test-report-srfi64
    "${scm_test_report_with_srfi64_SOURCES}"
    "tests"
    "${GUILE_DEPENDS};scm-srfi64-extras"
    FALSE
    )

  gnc_add_scheme_tests ("${scm_test_report_with_srfi64_SOURCES}")
endif (HAVE_SRFI64)

gnc_add_scheme_test_targets(scm-test-report
  "test-report-extras.scm"
  "tests"
  "${GUILE_DEPENDS}"
  FALSE
  )

gnc_add_scheme_test_targets(scm-test-report-2
  "${scm_test_report_SOURCES}"
  "tests"
  "scm-test-report"
  FALSE
)

add_dependencies(check scm-test-report)
set_dist_list(test_report_DIST
  CMakeLists.txt
  ${scm_test_report_with_srfi64_SOURCES}
  ${scm_test_report_SOURCES}
  test-report-extras.scm
  test-link-module.c
)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("tests test-report-system-extras" "tests test-report-extras" "scm-test-report" "")
