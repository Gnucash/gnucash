set(scm_test_SCHEME
  test-libgnucash-scm-utilities.scm
  test-module-deprecation.scm
  )

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-scm scm-engine
  scm-srfi64-extras
  scm-deprecated-gnucash-deprecated-module
  scm-deprecated-gnucash-superseded-module
  )

if (HAVE_SRFI64)
  gnc_add_scheme_test_targets(scm-test-scm
    "${scm_test_SCHEME}"
    "tests"
    "${GUILE_DEPENDS}"
    FALSE
    )

  gnc_add_scheme_tests("${scm_test_SCHEME}")

endif (HAVE_SRFI64)

set_dist_list(test_scm_DIST
  CMakeLists.txt
  ${scm_test_SCHEME}
  )

# Define two imaginary deprecated guile modules to test the compat file generation code
gnc_add_scheme_deprecated_module ("gnucash deprecated-module" "" "" "")
gnc_add_scheme_deprecated_module ("gnucash superseded-module" "gnucash utilities" "scm-scm" "")
