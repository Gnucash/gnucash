# Scheme

SET(utility_reports_SCHEME
  hello-world.scm
  utility-reports.scm
  view-column.scm
  welcome-to-gnucash.scm
)

SET(GUILE_OUTPUT_DIR     gnucash/report)
SET(GUILE_MODULES        libgnucash/app-utils libgnucash/engine gnucash/gnome-utils gnucash/html gnucash/report/report-system)
SET(GUILE_LOAD_DIRS      libgnucash/app-utils libgnucash/core-utils libgnucash/engine libgnucash/gnc-module
                            gnucash/gnome-utils gnucash/report/report-system libgnucash/scm)
SET(GUILE_LIBRARY_DIRS   libgnucash/engine libgnucash/app-utils libgnucash/core-utils libgnucash/gnc-module gnucash/gnome-utils gnucash/html
                              gnucash/report/report-system)
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
