set(support_DATA
  taxinvoice.eguile.scm
  taxinvoice.css
  receipt.eguile.scm
  receipt.css
  balsheet-eg.css
  balsheet-eg.eguile.scm
)

install(FILES ${support_DATA} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/gnucash/report)

set(support_EXTRA_DIST
  README
)

set_local_dist(reports_support_DIST_local CMakeLists.txt ${support_DATA} ${support_EXTRA_DIST})
set(reports_support_DIST ${reports_support_DIST_local} PARENT_SCOPE)
