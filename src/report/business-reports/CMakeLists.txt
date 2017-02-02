
SET (business_reports_SCHEME
  aging.scm
  customer-summary.scm
  easy-invoice.scm
  fancy-invoice.scm
  taxinvoice.scm
  invoice.scm
  job-report.scm
  owner-report.scm
  payables.scm
  receivables.scm
  balsheet-eg.scm
  business-reports.scm
)

SET(business_reports_DATA
  taxinvoice.eguile.scm
  taxinvoice.css
  balsheet-eg.css
  balsheet-eg.eguile.scm
)

SET(GUILE_OUTPUT_DIR   gnucash/report)
SET(GUILE_MODULES      src/app-utils src/engine src/html src/gnome-utils
                         src/report/report-system src/report/standard-reports)
SET(GUILE_LOAD_DIRS    src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/scm
                         src/report/business_reports src/report/report-system src/report/standard-reports)
SET(GUILE_LIBRARY_DIRS src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/report/report-system
                         src/report/standard-reports)
SET(GUILE_DEPENDS      scm-business-reports-links gncmod-standard-reports scm-standard-reports)

GNC_ADD_SCHEME_TARGETS(scm-business-reports
  "${business_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR} 
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  TRUE
)

INSTALL(FILES ${business_reports_DATA} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/gnucash/report)

SET_DIST_LIST(business_reports_DIST CMakeLists.txt Makefile.am ${business_reports_SCHEME} ${business_reports_DATA})