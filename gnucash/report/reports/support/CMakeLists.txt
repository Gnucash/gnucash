set(stylesheets_DATA
  balsheet-eg.css
  receipt.css
  owner-billing.css
  taxinvoice.css
)

install(FILES ${stylesheets_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/stylesheets)

set(templates_DATA
  balsheet-eg.eguile.scm
  receipt.eguile.scm
  owner-billing.eguile.scm
  taxinvoice.eguile.scm
)

install(FILES ${templates_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/templates)

set(support_EXTRA_DIST
  README
)

set_local_dist(reports_support_DIST_local CMakeLists.txt
  ${stylesheets_DATA} ${templates_DATA} ${support_EXTRA_DIST})
set(reports_support_DIST ${reports_support_DIST_local} PARENT_SCOPE)
