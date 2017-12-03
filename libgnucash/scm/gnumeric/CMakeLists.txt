

SET(gnumeric_SCHEME gnumeric-utilities.scm table-utils.scm)


GNC_ADD_SCHEME_TARGETS(scm-gnumeric
  "${gnumeric_SCHEME}"
  gnumeric
  ""
  FALSE
)

SET_DIST_LIST(scm_gnumeric_DIST CMakeLists.txt ${gnumeric_SCHEME})
