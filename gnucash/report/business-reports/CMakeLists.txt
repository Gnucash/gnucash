set(business_reports_DATA
  taxinvoice.eguile.scm
  taxinvoice.css
  receipt.eguile.scm
  receipt.css
  balsheet-eg.css
  balsheet-eg.eguile.scm
)

install(FILES ${business_reports_DATA} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/gnucash/report)

set_local_dist(business_reports_DIST_local CMakeLists.txt
  ${business_reports_DATA})
set(business_reports_DIST ${business_reports_DIST_local} PARENT_SCOPE)
