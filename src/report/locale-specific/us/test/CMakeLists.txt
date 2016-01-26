SET(LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS "")
SET(LOCALE_SPECIFIC_US_TEST_LIBS)

GNC_ADD_TEST(test-link-module-report-locale-specific-us test-link-module.c
  LOCALE_SPECIFIC_US_TEST_INCLUDE_DIRS LOCALE_SPECIFIC_US_TEST_LIBS
)

# This test not run in the autotools build.
#GNC_ADD_SCHEME_TEST(test-load-module-report-locale-specific-us
#  test-load-module.in
#)