set(LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS "")
set(LOCALE_SPECIFIC_US_TEST_LIBS)

gnc_add_test(test-link-module-report-locale-specific-us test-link-module.c
  LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS LOCALE_SPECIFIC_US_TEST_LIBS
)

# FIXME why is this not run/running?
# Answer: Bitrot. The test needs to be rewritten.
#set(GUILE_DEPENDS
#  scm-app-utils
#  gnc-core-utils
#  gnc-module
#  gncmod-engine
#)
#
#gnc_add_scheme_targets(test-load-report-locale-specific-module
#  "test-load-report-locale-specific-module.scm"
#  "gnucash/report"
#  "${GUILE_DEPENDS}"
#  FALSE
#  )
#gnc_add_scheme_tests(test-load-report-locale-specific-us-module.scm)

set_dist_list(test_locale_specific_us_DIST CMakeLists.txt test-link-module.c test-load-report-locale-specific-module.scm)
