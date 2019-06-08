set (scm-guile-json-details
  json/builder.scm
  json/parser.scm
  json/syntax.scm
)

gnc_add_scheme_targets (guile-json-details
  "${scm-guile-json-details}"
  gnucash/json
  ""
  TRUE
)

gnc_add_scheme_targets (guile-json
  json.scm
  "gnucash"
  "guile-json-details"
  FALSE
)

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
