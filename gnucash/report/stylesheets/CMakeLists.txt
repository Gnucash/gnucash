
set(stylesheets_SCHEME
  plain.scm
  footer.scm
  head-or-tail.scm
)

set(GUILE_OUTPUT_DIR gnucash/report/stylesheets)
set(GUILE_DEPENDS
  scm-gnc-module
  scm-core-utils
  scm-gettext
  scm-engine
  gncmod-html
  scm-report
  )

gnc_add_scheme_targets(scm-report-stylesheets-1
  "${stylesheets_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("gnucash report stylesheet-easy" "gnucash report stylesheets footer" "scm-report-stylesheets-1" "")
gnc_add_scheme_deprecated_module ("gnucash report stylesheet-fancy" "gnucash report stylesheets footer" "scm-report-stylesheets-1" "")
gnc_add_scheme_deprecated_module ("gnucash report stylesheet-footer" "gnucash report stylesheets footer" "scm-report-stylesheets-1" "")
gnc_add_scheme_deprecated_module ("gnucash report stylesheet-head-or-tail" "gnucash report stylesheets head-or-tail" "scm-report-stylesheets-1" "")
gnc_add_scheme_deprecated_module ("gnucash report stylesheet-plain" "gnucash report stylesheets plain" "scm-report-stylesheets-1" "")

add_custom_target(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1)

set_local_dist(stylesheets_DIST_local CMakeLists.txt ${stylesheets_SCHEME})

set(stylesheets_DIST ${stylesheets_DIST_local} PARENT_SCOPE)
