set(GUILE_DEPENDS
  scm-test-core
  scm-gettext
  scm-core-utils
  scm-gnc-module
  scm-scm
  scm-engine
  gncmod-backend-xml
  scm-gnome-utils
  gncmod-html
  scm-report-system
  scm-report-gnome
  scm-report-stylesheets
)

gnc_add_scheme_targets(scm-test-load-report-stylesheets-module
  "test-load-report-stylesheets-module.scm"
  "gnucash/report"
  "${GUILE_DEPENDS}"
  FALSE
  )

gnc_add_scheme_tests("test-load-report-stylesheets-module.scm")

set_dist_list(test_stylesheets_DIST CMakeLists.txt test-load-report-stylesheets-module.scm)
