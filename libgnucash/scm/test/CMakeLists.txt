set(scm_test_SCHEME
  test-utilities.scm
  )

set(GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-scm scm-engine
  )

if (HAVE_SRFI64)
  gnc_add_scheme_targets(scm-test-scm
    "${scm_test_SCHEME}"
    ""
    "${GUILE_DEPENDS}"
    FALSE
    )

  set(srfi64_extras_SCHEME_DIST
    srfi64-extras.scm
    )
  gnc_add_scheme_tests("${scm_test_SCHEME}")

endif (HAVE_SRFI64)

set_dist_list(test_scm_DIST
  CMakeLists.txt
  ${scm_test_SCHEME}
  )
