set(reports_DATA
  taxinvoice.eguile.scm
  taxinvoice.css
  receipt.eguile.scm
  receipt.css
  balsheet-eg.css
  balsheet-eg.eguile.scm
)

install(FILES ${reports_DATA} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/gnucash/report)

set_local_dist(reports_data_DIST_local CMakeLists.txt ${reports_DATA})
set(reports_data_DIST ${reports_data_DIST_local} PARENT_SCOPE)
