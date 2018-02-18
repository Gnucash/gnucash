SET(GUILE_DEPENDS
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

GNC_ADD_SCHEME_TARGETS(scm-test-load-report-stylesheets-module
  "test-load-report-stylesheets-module.scm"
  "gnucash/report"
  "${GUILE_DEPENDS}"
  FALSE
  )

GNC_ADD_SCHEME_TESTS("test-load-report-stylesheets-module.scm")

SET_DIST_LIST(test_stylesheets_DIST CMakeLists.txt test-load-report-stylesheets-module.scm)
