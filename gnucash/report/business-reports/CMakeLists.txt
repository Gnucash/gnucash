
SET (business_reports_SCHEME
  aging.scm
  customer-summary.scm
  easy-invoice.scm
  fancy-invoice.scm
  taxinvoice.scm
  receipt.scm
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
  receipt.eguile.scm
  receipt.css
  balsheet-eg.css
  balsheet-eg.eguile.scm
)

SET(GUILE_OUTPUT_DIR   gnucash/report)
SET(GUILE_DEPENDS      scm-business-reports-links scm-standard-reports)

GNC_ADD_SCHEME_TARGETS(scm-business-reports
  "${business_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

INSTALL(FILES ${business_reports_DATA} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/gnucash/report)

SET_DIST_LIST(business_reports_DIST CMakeLists.txt ${business_reports_SCHEME} ${business_reports_DATA})
