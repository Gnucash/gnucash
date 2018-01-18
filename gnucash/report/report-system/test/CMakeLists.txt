
SET(REPORT_SYSTEM_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}
)
SET(REPORT_SYSTEM_TEST_LIBS gnc-module)

GNC_ADD_TEST_WITH_GUILE(test-link-module-report-system test-link-module.c
  REPORT_SYSTEM_TEST_INCLUDE_DIRS REPORT_SYSTEM_TEST_LIBS
)

SET(scm_test_report_system_SOURCES
  test-load-report-system-module.scm
  test-collectors.scm
  test-list-extras.scm
  test-report-utilities.scm
# test-test-extras.scm ;;Not run in autotools
  )

GNC_ADD_SCHEME_TESTS(${scm_test_report_system_SOURCES})

GNC_ADD_SCHEME_TARGETS(scm-test-report-system
  "test-extras.scm"
  gnucash/report/report-system/test
  "scm-gnc-module;scm-test-engine;scm-report-system-3"
  FALSE
  )

GNC_ADD_SCHEME_TARGETS(scm-test-report-system-2
  "${scm_test_report_system_SOURCES}"
  gnucash/report/report-system/test
  "scm-test-report-system"
  FALSE
)


SET_DIST_LIST(test_report_system_DIST
  CMakeLists.txt
  
  ${scm_test_report_system_SOURCES}
  test-extras.scm
  test-link-module.c
  test-test-extras.scm
)
