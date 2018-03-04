
GNC_ADD_SCHEME_TARGETS(guile-json
  json.scm
  json/builder.scm
  json/parser.scm
  json/syntax.scm
)

SET_DIST_LIST(guile-json_DIST CMakeLists.txt ${guile-json_DATA})
