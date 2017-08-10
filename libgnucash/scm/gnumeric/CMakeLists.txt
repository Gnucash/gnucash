

SET(gnumeric_SCHEME gnumeric-utilities.scm table-utils.scm)


SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    "")
SET(GUILE_LIBRARY_DIRS "")
SET(GUILE_DEPENDS      "")

GNC_ADD_SCHEME_TARGETS(scm-gnumeric
  "${gnumeric_SCHEME}"
  gnumeric
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

SET_DIST_LIST(scm_gnumeric_DIST CMakeLists.txt Makefile.am ${gnumeric_SCHEME})
