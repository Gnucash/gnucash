set(LOCALE_TAX_TEST_INCLUDE_DIRS "")
set(LOCALE_TAX_TEST_LIBS "")

gnc_add_test(test-link-module-tax-us test-link-module.c
  LOCALE_TAX_TEST_INCLUDE_DIRS LOCALE_TAX_TEST_LIBS
)

# FIXME why is this test not run ?
#set(GUILE_DEPENDS
#  gnc-core-utils
#  scm-gnc-module
#)
#
#gnc_add_scheme_test_targets(scm-test-load-tax-us-module
#  "test-load-locale-tax-module.scm"
#  "tests"
#  "${GUILE_DEPENDS}"
#  FALSE
#)
#gnc_add_scheme_test(test-load-locale-tax-module.scm)

set_dist_list(test_locale_tax_DIST CMakeLists.txt test-link-module.c test-load-locale-tax-module.scm)
