SET(TAX_US_TEST_INCLUDE_DIRS "")
SET(TAX_US_TEST_LIBS "")

GNC_ADD_TEST(test-link-module-tax-us test-link-module.c
  TAX_US_TEST_INCLUDE_DIRS TAX_US_TEST_LIBS
)

# This test not run in autotools build.
#SET(GUILE_DEPENDS
#  gnc-core-utils
#  gnc-module
#)
#
#GNC_ADD_SCHEME_TARGETS(scm-test-load-tax-us-module
#  "test-load-tax-us-module.scm"
#  "gnucash/reports"
#  "${GUILE_DEPENDS}"
#  FALSE
#)
#GNC_ADD_SCHEME_TEST(test-load-tax-us-module.scm)

SET_DIST_LIST(test_tax_us_DIST CMakeLists.txt test-link-module.c test-load-tax-us-module.scm)
