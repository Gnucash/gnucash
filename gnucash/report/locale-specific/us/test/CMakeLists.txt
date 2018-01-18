SET(LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS "")
SET(LOCALE_SPECIFIC_US_TEST_LIBS)

GNC_ADD_TEST(test-link-module-report-locale-specific-us test-link-module.c
  LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS LOCALE_SPECIFIC_US_TEST_LIBS
)

# This test not run in the autotools build.
#SET(GUILE_DEPENDS
#  gncmod-app-utils
#  gnc-core-utils
#  gnc-module
#  gncmod-engine
#)
#
#GNC_ADD_SCHEME_TARGETS(test-load-report-locale-specific-module
#  "test-load-report-locale-specific-module.scm"
#  "gnucash/report"
#  "${GUILE_DEPENDS}"
#  FALSE
#  )
#GNC_ADD_SCHEME_TESTS(test-load-report-locale-specific-us-module.scm)

SET_DIST_LIST(test_locale_specific_us_DIST CMakeLists.txt test-link-module.c test-load-report-locale-specific-module.scm)
