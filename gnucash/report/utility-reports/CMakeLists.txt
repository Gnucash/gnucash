# Scheme

set(utility_reports_SCHEME
  hello-world.scm
  utility-reports.scm
  view-column.scm
  welcome-to-gnucash.scm
)

set(GUILE_OUTPUT_DIR     gnucash/report)
set(GUILE_DEPENDS
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

gnc_add_scheme_targets(scm-utility-reports
  "${utility_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

set_local_dist(utility_reports_DIST_local CMakeLists.txt
        ${utility_reports_SCHEME})
set(utility_reports_DIST ${utility_reports_DIST_local} ${test_utility_reports_DIST} PARENT_SCOPE)
