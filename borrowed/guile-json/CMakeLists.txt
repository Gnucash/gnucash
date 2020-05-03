add_subdirectory(json)

gnc_add_scheme_targets (guile-json
  json.scm
  ""
  ""
  FALSE
)

set_local_dist(guile-json_DIST_LOCAL
  AUTHORS
  CMakeLists.txt
  COPYING
  json.scm
  NEWS
  README.org
)

set(guile-json_DIST ${guile-json_DIST_LOCAL} ${guile-json-details_DIST} PARENT_SCOPE)
