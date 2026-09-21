
set(scm_qifimp_test_with_srfi64_SOURCES
  test-qif-imp.scm
  test-qif-parse.scm
  test-qif-merge-groups.scm
  test-qif-date-formats.scm
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
  set_property(TEST test-qif-date-formats APPEND PROPERTY ENVIRONMENT
    "QIF_DATE_FIXTURES=${CMAKE_CURRENT_SOURCE_DIR}/date-formats"
    "GNC_UNINSTALLED=1"
    "GNC_BUILDDIR=${CMAKE_BINARY_DIR}")

  gnc_add_scheme_test_targets(scm-test-qif-imp-srfi64
    SOURCES "${scm_qifimp_test_with_srfi64_SOURCES}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")
  add_dependencies(check scm-qif-import-2 scm-qif-import)
endif()

set_dist_list(test_qif_import_DIST CMakeLists.txt
  ${scm_qifimp_test_with_srfi64_SOURCES}
  date-formats/README.md
  date-formats/iso-multi-day31-first.qif
  date-formats/iso-multi-day31-last.qif
  date-formats/iso-multi-days-at-most12.qif
  date-formats/iso-multi-equal-month-day.qif
  date-formats/iso-multi-prices-ambiguous.qif
  date-formats/iso-multi-prices-day31.qif
  date-formats/ymd-slash.qif
  date-formats/ymd-hyphen.qif
  date-formats/ymd-dot.qif
  date-formats/ymd-apostrophe.qif
  date-formats/ymd-compact.qif
  date-formats/ymd-day13.qif
  date-formats/ydm-day13.qif
  date-formats/year-first-equal-parts.qif
  date-formats/mixed-price-ymd-transaction-mdy.qif
  date-formats/price-ymd-transaction-unambiguous.qif
  date-formats/price-and-transaction-ymd.qif
  date-formats/inconsistent-per-record-formats.qif)
