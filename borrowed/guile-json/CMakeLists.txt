set (scm-guile-json-details
  json/builder.scm
  json/parser.scm
  json/syntax.scm
)

gnc_add_scheme_targets (guile-json-details
    SOURCES "${scm-guile-json-details}"
    OUTPUT_DIR gnucash/json
    MAKE_LINKS)

gnc_add_scheme_targets (guile-json
    SOURCES json.scm
    OUTPUT_DIR "gnucash"
    DEPENDS "guile-json-details")

set_local_dist(guile-json_DIST_LOCAL
  AUTHORS
  CMakeLists.txt
  COPYING
  json.scm
  ${scm-guile-json-details}
  NEWS
  README.org
)

set(guile-json_DIST ${guile-json_DIST_LOCAL} PARENT_SCOPE)
