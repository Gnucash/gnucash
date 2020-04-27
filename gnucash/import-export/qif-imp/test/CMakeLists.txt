
set(scm_qifimp_test_with_srfi64_SOURCES
  test-qif-imp.scm
  test-qif-parse.scm
  test-qif-merge-groups.scm
  )

set (GUILE_DEPENDS
  scm-app-utils
  scm-engine
  scm-test-engine
  scm-core-utils
  scm-qif-import-2
)

if (HAVE_SRFI64)
  gnc_add_scheme_tests("${scm_qifimp_test_with_srfi64_SOURCES}")

  gnc_add_scheme_test_targets(scm-test-qif-imp-srfi64
    SOURCES "${scm_qifimp_test_with_srfi64_SOURCES}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")
  add_dependencies(check scm-qif-import-2 scm-qif-import)
endif()

set_dist_list(test_qif_import_DIST CMakeLists.txt
  ${scm_qifimp_test_with_srfi64_SOURCES})
