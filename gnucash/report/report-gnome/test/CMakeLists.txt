set(REPORT_GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}

)


set(REPORT_GNOME_TEST_LIBS gnc-module)

gnc_add_test_with_guile(test-link-module-report-gnome test-link-module.c
  REPORT_GNOME_TEST_INCLUDE_DIRS REPORT_GNOME_TEST_LIBS
)

set(GUILE_DEPENDS
  scm-app-utils
  scm-gnome-utils
  gnc-module
  scm-engine
  scm-report-gnome
  scm-business-reports
)

add_custom_target(test-report-gnome ALL DEPENDS ${GUILE_DEPENDS})
add_dependencies(test-link-module-report-gnome test-report-gnome)

gnc_add_scheme_targets(scm-test-load-report-gnome-module
  "test-load-report-gnome-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)
gnc_add_scheme_tests(test-load-report-gnome-module.scm)

set_dist_list(test_report_gnome_DIST CMakeLists.txt test-link-module.c test-load-report-gnome-module.scm)

