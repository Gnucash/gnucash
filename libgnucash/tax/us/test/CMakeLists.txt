set(TAX_US_TEST_INCLUDE_DIRS "")
set(TAX_US_TEST_LIBS "")

gnc_add_test(test-link-module-tax-us test-link-module.c
  TAX_US_TEST_INCLUDE_DIRS TAX_US_TEST_LIBS
)

# FIXME why is this test not run ?
#set(GUILE_DEPENDS
#  gnc-core-utils
#  scm-gnc-module
#)
#
#gnc_add_scheme_targets(scm-test-load-tax-us-module
#  "test-load-tax-us-module.scm"
#  "gnucash/reports"
#  "${GUILE_DEPENDS}"
#  FALSE
#)
#gnc_add_scheme_test(test-load-tax-us-module.scm)

set_dist_list(test_tax_us_DIST CMakeLists.txt test-link-module.c test-load-tax-us-module.scm)
