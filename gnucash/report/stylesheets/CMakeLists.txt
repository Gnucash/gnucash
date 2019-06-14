
set(stylesheets_SCHEME
  plain.scm
  fancy.scm
  footer.scm
  easy.scm
  head-or-tail.scm
)

set(GUILE_OUTPUT_DIR gnucash/report/stylesheets)
set(GUILE_DEPENDS
  scm-gnc-module
  scm-core-utils
  scm-gettext
  scm-scm
  gncmod-html
  scm-report
  )

gnc_add_scheme_targets(scm-report-stylesheets-1
  "${stylesheets_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

add_custom_target(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1)

set_local_dist(stylesheets_DIST_local CMakeLists.txt ${stylesheets_SCHEME})

set(stylesheets_DIST ${stylesheets_DIST_local} PARENT_SCOPE)
