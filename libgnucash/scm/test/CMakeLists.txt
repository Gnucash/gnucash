set(scm_test_SCHEME
  test-libgnucash-scm-utilities.scm
  )

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-scm scm-engine
  scm-srfi64-extras
  )

if (HAVE_SRFI64)
  gnc_add_scheme_targets(scm-test-scm
    "${scm_test_SCHEME}"
    ""
    "${GUILE_DEPENDS}"
    FALSE
    )

  gnc_add_scheme_tests("${scm_test_SCHEME}")

endif()

set_dist_list(test_scm_DIST
  CMakeLists.txt
  ${scm_test_SCHEME}
  )
