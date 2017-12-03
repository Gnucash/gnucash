SET(GUILE_DEPENDS
  test-core
  gncmod-app-utils
  gnc-core-utils
  gnc-module
  gncmod-engine
  gncmod-backend-xml
  gncmod-backend-xml
  gncmod-gnome-utils
  gncmod-html
  gncmod-report-system
  gncmod-report-gnome
)

GNC_ADD_SCHEME_TARGETS(scm-test-load-report-stylesheets-module
  "test-load-report-stylesheets-module.scm"
  "gnucash/report"
  "${GUILE_DEPENDS}"
  FALSE
  )

GNC_ADD_SCHEME_TESTS("test-load-report-stylesheets-module.scm")

SET_DIST_LIST(test_stylesheets_DIST CMakeLists.txt test-load-report-stylesheets-module.scm)
