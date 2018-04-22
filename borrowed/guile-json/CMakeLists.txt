set (guile-json_SCHEME
  json.scm
  json/builder.scm
  json/parser.scm
  json/syntax.scm
  )

gnc_add_scheme_targets (guile-json
  "${guile-json_SCHEME}"
  ""
  ""
  FALSE
  )

set_dist_list (guile-json_DIST
  CMakeLists.txt
  ${guile-json_SCHEME}
  )
