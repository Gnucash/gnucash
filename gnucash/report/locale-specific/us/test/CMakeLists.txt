set(LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS "")
set(LOCALE_SPECIFIC_US_TEST_LIBS)

gnc_add_test(test-link-module-report-locale-specific-us test-link-module.c
  LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS LOCALE_SPECIFIC_US_TEST_LIBS
)

set(GUILE_DEPENDS
  scm-app-utils
  gnc-core-utils
  gnc-module
  gncmod-engine
)

gnc_add_scheme_test_targets(test-load-report-locale-specific-module
  "test-load-report-locale-specific-module.scm"
  "tests"
  "${GUILE_DEPENDS}"
  FALSE
  )
gnc_add_scheme_tests(test-load-report-locale-specific-module.scm)

set_dist_list(test_locale_specific_us_DIST CMakeLists.txt test-link-module.c test-load-report-locale-specific-module.scm)
