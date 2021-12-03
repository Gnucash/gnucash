
set(stylesheets_SCHEME
  css.scm
  plain.scm
  footer.scm
  head-or-tail.scm
)

set(GUILE_DEPENDS
  scm-core-utils
  scm-core-utils
  scm-engine
  gnc-html
  scm-report
  )

gnc_add_scheme_targets(scm-report-stylesheets-1
    SOURCES "${stylesheets_SCHEME}"
    OUTPUT_DIR "gnucash/report/stylesheets"
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

add_custom_target(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1)

set_local_dist(stylesheets_DIST_local CMakeLists.txt ${stylesheets_SCHEME})

set(stylesheets_DIST ${stylesheets_DIST_local} PARENT_SCOPE)
