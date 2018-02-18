# Scheme

SET(utility_reports_SCHEME
  hello-world.scm
  utility-reports.scm
  view-column.scm
  welcome-to-gnucash.scm
)

SET(GUILE_OUTPUT_DIR     gnucash/report)
SET(GUILE_DEPENDS
  gncmod-html
  scm-app-utils
  scm-core-utils
  scm-gettext
  scm-gnc-module
  scm-scm
  scm-utility-reports-links
  scm-gnome-utils
  scm-report-system
  )

GNC_ADD_SCHEME_TARGETS(scm-utility-reports
  "${utility_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

SET_LOCAL_DIST(utility_reports_DIST_local CMakeLists.txt 
        ${utility_reports_SCHEME})
SET(utility_reports_DIST ${utility_reports_DIST_local} ${test_utility_reports_DIST} PARENT_SCOPE)
