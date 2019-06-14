set(stylesheets_DATA
  taxinvoice.css
  receipt.css
  balsheet-eg.css
)

install(FILES ${stylesheets_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/stylesheets)

set(templates_DATA
  taxinvoice.eguile.scm
  receipt.eguile.scm
  balsheet-eg.eguile.scm
)

install(FILES ${templates_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/templates)

set(support_EXTRA_DIST
  README
)

set_local_dist(reports_support_DIST_local CMakeLists.txt
  ${stylesheets_DATA} ${templates_DATA} ${support_EXTRA_DIST})
set(reports_support_DIST ${reports_support_DIST_local} PARENT_SCOPE)
