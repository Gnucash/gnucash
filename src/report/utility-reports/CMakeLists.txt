# Scheme

SET(utility_reports_SCHEME
  hello-world.scm
  utility-reports.scm
  view-column.scm
  welcome-to-gnucash.scm
)

SET(GUILE_OUTPUT_DIR     gnucash/report)
SET(GUILE_MODULES        src/app-utils src/engine src/gnome-utils src/html src/report/report-system)
SET(GUILE_LOAD_DIRS      src/app-utils src/core-utils src/engine src/gnc-module
                            src/gnome-utils src/report/report-system src/scm)
SET(GUILE_LIBRARY_DIRS   src/engine src/app-utils src/core-utils src/gnc-module src/gnome-utils src/html
                              src/report/report-system)
SET(GUILE_DEPENDS        scm-utility-reports-links scm-app-utils scm-report-system-3)

GNC_ADD_SCHEME_TARGETS(scm-utility-reports
  "${utility_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  TRUE
)

SET_LOCAL_DIST(utility_reports_DIST_local CMakeLists.txt Makefile.am
        ${utility_reports_SCHEME})
SET(utility_reports_DIST ${utility_reports_DIST_local} ${test_utility_reports_DIST} PARENT_SCOPE)
