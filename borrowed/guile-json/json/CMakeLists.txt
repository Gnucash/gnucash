set (scm-guile-json-details
  builder.scm
  parser.scm
  syntax.scm
)

gnc_add_scheme_targets (guile-json-details
  "${scm-guile-json-details}"
  json
  ""
  FALSE
)

set_dist_list (guile-json-details_DIST
  CMakeLists.txt
  ${scm-guile-json-details}
)
