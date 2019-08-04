

set(QIF_IMP_TEST_INCLUDE_DIRS)
set(QIF_IMP_TEST_LIBS)

set(scm_qifimp_test_with_srfi64_SOURCES
  test-qif-imp.scm
  test-qif-parse.scm
  test-qif-merge-groups.scm
  )

set (GUILE_DEPENDS
  scm-gnc-module
  scm-app-utils
  scm-engine
  scm-test-engine
  scm-gettext
  scm-scm
  scm-qif-import
)
gnc_add_test(test-link-qif-imp test-link.c QIF_IMP_TEST_INCLUDE_DIRS QIF_IMP_TEST_LIBS)

if (HAVE_SRFI64)
  gnc_add_scheme_tests("${scm_qifimp_test_with_srfi64_SOURCES}")

  gnc_add_scheme_test_targets(scm-test-qif-imp-srfi64
    "${scm_qifimp_test_with_srfi64_SOURCES}"
    "tests"
    "${GUILE_DEPENDS};scm-srfi64-extras"
    FALSE
    )
endif (HAVE_SRFI64)

set_dist_list(test_qif_import_DIST CMakeLists.txt test-link.c
  ${scm_qifimp_test_with_srfi64_SOURCES})
