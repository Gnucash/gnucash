
set(REPORT_SYSTEM_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}
)
set(REPORT_SYSTEM_TEST_LIBS gnc-module)

gnc_add_test_with_guile(test-link-module-report-system test-link-module.c
  REPORT_SYSTEM_TEST_INCLUDE_DIRS REPORT_SYSTEM_TEST_LIBS
)

set(scm_test_report_system_SOURCES
  test-load-report-system-module.scm
  test-test-extras.scm
)

set (scm_test_report_system_with_srfi64_SOURCES
  test-commodity-utils.scm
  test-report-utilities.scm
  test-html-utilities-srfi64.scm
  test-html-fonts.scm
  test-report-html.scm
  test-report-system.scm
  )

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-core-utils
  scm-gettext
  scm-engine
  scm-test-engine
  scm-scm
  scm-report-system-3
  )
gnc_add_scheme_tests("${scm_test_report_system_SOURCES}")

if (HAVE_SRFI64)
  gnc_add_scheme_tests ("${scm_test_report_system_with_srfi64_SOURCES}")
endif()

gnc_add_scheme_targets(scm-test-report-system
  "test-extras.scm"
  gnucash/report/report-system/test
  "${GUILE_DEPENDS}"
  FALSE
  )

gnc_add_scheme_targets(scm-test-report-system-2
  "${scm_test_report_system_SOURCES}"
  gnucash/report/report-system/test
  "scm-test-report-system"
  FALSE
)

add_dependencies(check scm-test-report-system)
set_dist_list(test_report_system_DIST
  CMakeLists.txt
  ${scm_test_report_system_with_srfi64_SOURCES}
  ${scm_test_report_system_SOURCES}
  test-extras.scm
  test-link-module.c
)
