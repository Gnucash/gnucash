set(LOCALE_TAX_TEST_INCLUDE_DIRS ${GUILE_INCLUDE_DIRS})
set(LOCALE_TAX_TEST_LIBS
    gnc-engine
    gnc-locale-tax
    ${GUILE_LDFLAGS})

gnc_add_test_with_guile(test-link-module-tax-us test-link-module.c
LOCALE_TAX_TEST_INCLUDE_DIRS LOCALE_TAX_TEST_LIBS)

set_dist_list(test_locale_tax_DIST CMakeLists.txt test-link-module.c)
