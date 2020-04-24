
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

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report stylesheet-easy"
    NEW_MODULE "gnucash report stylesheets footer"
    DEPENDS "scm-report-stylesheets-1")
gnc_add_scheme_deprecated_module (
    OLD_MODULE  "gnucash report stylesheet-fancy"
    NEW_MODULE "gnucash report stylesheets footer"
    DEPENDS "scm-report-stylesheets-1")
gnc_add_scheme_deprecated_module (
    OLD_MODULE  "gnucash report stylesheet-footer"
    NEW_MODULE "gnucash report stylesheets footer"
    DEPENDS "scm-report-stylesheets-1")
gnc_add_scheme_deprecated_module (
    OLD_MODULE  "gnucash report stylesheet-head-or-tail"
    NEW_MODULE "gnucash report stylesheets head-or-tail"
    DEPENDS "scm-report-stylesheets-1")
gnc_add_scheme_deprecated_module (
    OLD_MODULE  "gnucash report stylesheet-plain"
    NEW_MODULE "gnucash report stylesheets plain"
    DEPENDS "scm-report-stylesheets-1")

add_custom_target(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1)

set_local_dist(stylesheets_DIST_local CMakeLists.txt ${stylesheets_SCHEME})

set(stylesheets_DIST ${stylesheets_DIST_local} PARENT_SCOPE)
